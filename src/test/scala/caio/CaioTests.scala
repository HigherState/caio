package caio

import caio.implicits.DynamicContextImplicits
import caio.mtl.InvariantAsk
import cats.effect.concurrent.Deferred
import cats.effect.{ContextShift, IO, LiftIO}

import scala.concurrent.ExecutionContext
import caio.std.CaioListen
import cats.mtl.{Listen, Tell}
import cats.Applicative
import cats.syntax.ApplicativeErrorOps
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioTests extends AsyncFunSpec with Matchers {

  import Event._

  type L = EventLog
  type C = Map[String, String]

  type CaioT[A] = Caio[C, L, A]

  implicit val CS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  type CaioC[C1, A] = Caio[C1, L, A]
  type CaioL[L1, A] = Caio[C, L1, A]

  val implicits = new DynamicContextImplicits[L]
  import implicits._

  /*val emptyState:Store[C, L] =
    EmptyStore
  val simpleState:Store[C, L] =
    ContentStore(Map("one" -> "one"), Vector(event1, event2), EventMonoid)
  val simpleState2:Store[C, L] =
    ContentStore(Map("two" -> "two"), Vector(event3), EventMonoid)

  val simpleState12:Store[C, L] =
    ContentStore(Map("two" -> "two"), Vector(event1, event2, event3), EventMonoid)*/

  describe("Stack safety") {
    it("should be stack-safe under flatMap") {
      def summation(n: Long): CaioT[Long] =
        if (n > 0) {
          for {
            a <- Applicative[CaioT].pure(n)
            c <- summation(n - 1)
          } yield a + c
        } else
          Applicative[CaioT].pure(n)

      summation(1000000).run(Map.empty).unsafeRunSync() shouldBe 500000500000L
    }

    it("should be stack-safe under flatmap with IO") {
      /* It appears we get stack safety on any call involving IO.
        Im assuming as that is because IO is stack safe though An outer call stack with
        inner stack safe calls, cant quite visualise why that would be the case.
       */
      def summation(n: Long): CaioT[Long] =
        if (n > 0) {
          for {
            a <- Applicative[CaioT].pure(n)
            b <- LiftIO[CaioT].liftIO[Long](IO.delay(a + 5))
            c <- summation(n - 1)
          } yield b + c
        } else
          Applicative[CaioT].pure(n)

      summation(100000).run(Map.empty).unsafeRunSync() shouldBe 5000550000L
    }
    it("should be stack-safe under flatmap with Kleisli") {
      def summation(n: Long): CaioT[Long] =
        if (n > 0) {
          for {
            a <- Applicative[CaioT].pure(n)
            c <- summation(n - 1)
            l <- KleisliCaio[C, L, Long](ctx => IO.pure(FoldCaioSuccess(ctx, EventMonoid.empty, a + c)))
          } yield l
        } else
          Applicative[CaioT].pure(n)

      summation(100000).run(Map.empty).unsafeRunSync() shouldBe 5000050000L
    }
  }

  describe("Error Handling") {
    it("Should catch the error when the completion of a Deferred fails") {

      val program =
        for {
          promise <- Deferred[CaioT, Unit]
          _       <- promise.complete(())
          either  <- new ApplicativeErrorOps(promise.complete(())).attempt
        } yield either

      program.run(Map.empty).unsafeRunSync().isLeft shouldBe true
    }
  }

  describe("Context") {
    it("Should run passing any value when no context required") {
      val program: CaioC[Any, Int] =
        for {
          a <- Applicative[CaioC[Any, *]].pure(1)
          b <- Applicative[CaioC[Any, *]].pure(2)
          c <- Applicative[CaioC[Any, *]].pure(3)
        } yield a + b + c

      program.run(()).unsafeRunSync() shouldBe 6
      program.run(false).unsafeRunSync() shouldBe 6
      program.run(1).unsafeRunSync() shouldBe 6
      program.run.unsafeRunSync() shouldBe 6
    }

    it("Should combine Any with a context") {
      val program: CaioT[Int] =
        for {
          a <- Applicative[CaioT].pure(1)
          b <- Applicative[CaioC[Any, *]].pure(2)
          c <- Applicative[CaioT].pure(3)
        } yield a + b + c

      program.run(Map.empty).unsafeRunSync() shouldBe 6
    }

    it("Should combine different contexts") {
      trait Context0 { def x: Int }
      trait Context1 { def y: Int }

      val program: CaioC[Context0 with Context1, Int] =
        for {
          c0 <- InvariantAsk[CaioC[Context0, *], Context0].ask[Context0]
          c1 <- InvariantAsk[CaioC[Context1, *], Context1].ask[Context1]
          c  <- Applicative[CaioC[Any, *]].pure(3)
        } yield c0.x + c1.y + c

      val context = new Context0 with Context1 { def x = 1; def y = 2; }

      program.run(context).unsafeRunSync() shouldBe 6
    }

    it("Should provide context") {
      val program: CaioC[Any, Int] =
        InvariantAsk[CaioC[(Int, Int, Int), *], (Int, Int, Int)]
          .ask[(Int, Int, Int)]
          .map { case (a, b, c) => a + b + c }
          .provideContext((1, 2, 3))

      program.run.unsafeRunSync() shouldBe 6
    }

    it("Should preserve error when using tapError") {
      case object CustomException extends Exception
      val program = (Caio.pure(5) *> Caio.raiseError(CustomException)).tapError(_ => Caio.unit)
      program.attempt.run(Map.empty).unsafeRunSync() shouldBe Left(CustomException)
    }
  }

  describe("Logging") {
    sealed trait LogEvent
    case object LogEvent0 extends LogEvent
    case object LogEvent1 extends LogEvent

    implicit def implicits[A] = new CaioListen[C, List[A]]

    it("Should combine different log events") {
      val program: CaioL[List[LogEvent], (Unit, List[LogEvent])] =
        Listen[CaioL[List[LogEvent], *], List[LogEvent]].listen {
          for {
            _ <- Tell[CaioL[List[LogEvent0.type], *], List[LogEvent0.type]].tell(List(LogEvent0))
            _ <- Tell[CaioL[List[LogEvent1.type], *], List[LogEvent1.type]].tell(List(LogEvent1))
          } yield ()
        }

      program.run(Map.empty).unsafeRunSync()._2 shouldBe List(LogEvent0, LogEvent1)
    }
  }

  //including context should effect tests
  /*def getResult[A](c:CaioT[A]):(Either[EoF, A], Store[C, L]) =
    c.toResult(Map.empty).toIO.unsafeRunSync() match {
      case SuccessResult(a, state) =>
        Right(a) -> state
      case ErrorResult(ex, state) =>
        Left(ex) -> state
    }

  private[caio] def toResult[A](t:EoF, s:Store[C, L]):(Either[EoF, A], Store[C, L]) =
    Left(t) -> s

  private[caio] def toResult[A](a:A, s:Store[C, L]):(Either[EoF, A], Store[C, L]) =
    Right(a) -> s


  describe("Preserving of state and result in For comprehension") {

    describe("A single operation") {
      it("Result inState") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }
      it("Result inError") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
          } yield a
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Right(failures1), simpleState))
          } yield a
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState))))
          } yield a
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }

    describe("Paired Operations") {

      it("Result in State,State") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioState[C, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, Error") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioError[C, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, State") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
            b <- CaioState[C, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in State, success Kleisli") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioKleisli[C, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioState[C, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, error Kleisli") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState))
            b <- CaioState[C, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in State, IO Success Kleisli") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioState[C, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, IO Error Kleisli") {
        val result =
          for {
            a <- CaioState[C, L, String]("value", simpleState)
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Right(failures1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState12)
      }
      it("Result in IO Error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Right(failures1), simpleState))))
            b <- CaioState[C, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in Error, Error") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
            b <- CaioError[C, L, Int](Left(exception2), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result inError, Kleisli") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioError[C, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, error Kleisli") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState))
            b <- CaioError[C, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in Error, IO Success Kleisli") {
        val result =
          for {
            a <- CaioError[C, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioError[C, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, IO Error Kleisli") {
        val result =
          for {
            a <- CaioError[C, L, Int](Right(failures1), simpleState)
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in IO Error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState))))
            b <- CaioError[C, L, Int](Right(failures1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }


      it("Result in success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Right(failures1), simpleState))
            b <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in IO Error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Right(failures1), simpleState))))
            b <- CaioKleisli[C, L, String](c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Right(failures1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Right(failures1), simpleState))))
            b <- CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in IO Success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in IO Error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState))))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }

      it("Result in IO Error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState))))
            b <- CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }
  }

  describe("Preserve under exception") {

    it("Preserve in Kleisli") {
      val result =
        CaioKleisli[C, L, String](c => throw exception1)
      getResult(result) shouldBe toResult(Left(exception1), emptyState)
    }

    describe("Under a flat map") {

      it("Preserve in State") {
        val result =
          CaioState[C, L, String]("value", simpleState).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[C, L, Int](Left(exception1), simpleState).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => SuccessResult("value", simpleState)).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState)).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState)))).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState)))).flatMap(_ => throw exception2)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }

    describe("Under a map") {
      it("Preserve in State") {
        val result =
          CaioState[C, L, String]("value", simpleState).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[C, L, Int](Left(exception1), simpleState).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => SuccessResult("value", simpleState)).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli[C, L, Int](c => ErrorResult[C, L, Int](Left(exception1), simpleState)).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState)))).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli[C, L, String](c => IOResult(IO.delay(ErrorResult[C, L, String](Left(exception1), simpleState)))).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }

  }*/
}
