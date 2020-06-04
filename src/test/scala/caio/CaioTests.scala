package caio

import caio.implicits.StaticImplicits
import caio.mtl.ApplicativeFail
import cats.effect.{IO, LiftIO}
//import caio.std.CaioBaselineInstances
import cats.Applicative
import cats.Monad.ops._
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioTests extends AsyncFunSpec with Matchers {

  import Event._
  //import Exception._
  //import Failure._

  type L = Vector[Event]
  type C = Map[String, String]
  type V = Failure


  type CaioT[A] = Caio[C, Failure, EventLog, A]

  val implicits = new StaticImplicits[C, V, L]()
  import implicits._
  

  /*val emptyState:Store[C, L] =
    EmptyStore
  val simpleState:Store[C, L] =
    ContentStore(Map("one" -> "one"), Vector(event1, event2), EventMonoid)
  val simpleState2:Store[C, L] =
    ContentStore(Map("two" -> "two"), Vector(event3), EventMonoid)

  val simpleState12:Store[C, L] =
    ContentStore(Map("two" -> "two"), Vector(event1, event2, event3), EventMonoid)*/

  describe("Caio") {
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
      summation(100000).run(Map.empty).unsafeRunSync()shouldBe 5000550000L
    }
    it("should be stack-safe under flatmap with Kleisli") {
      def summation(n: Long): CaioT[Long] =
        if (n > 0) {
         for {
            a <- Applicative[CaioT].pure(n)
            c <- summation(n - 1)
            k:CaioT[Long] = KleisliCaio[C, V, L, Long]{ ctx => FoldCaioIO(IO.pure(FoldCaioSuccess(ctx, EventMonoid.empty, a + c)))}
            l <- k
          } yield l
        } else
          Applicative[CaioT].pure(n)
      summation(100000).run(Map.empty).unsafeRunSync()  shouldBe 5000050000L
    }

    it("should be stack-safe under flatmap with handle Failures") {
      def summation(n: Long): CaioT[Long] =
        if (n > 0) {

          val cr = for {
            a <- Applicative[CaioT].pure(n)
            c <- summation(n - 1)
          } yield a + c
          ApplicativeFail[CaioT, V].handleFailuresWith(cr)(_ => Applicative[CaioT].pure(0L))
        } else
          Applicative[CaioT].pure(n)
      summation(100000).run(Map.empty).unsafeRunSync() shouldBe 5000050000L
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
            a <- CaioState[C, V, L, String]("value", simpleState)
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }
      it("Result inError") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
          } yield a
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Right(failures1), simpleState))
          } yield a
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState))))
          } yield a
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }

    describe("Paired Operations") {

      it("Result in State,State") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioState[C, V, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, Error") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioError[C, V, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, State") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
            b <- CaioState[C, V, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in State, success Kleisli") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioState[C, V, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, error Kleisli") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState))
            b <- CaioState[C, V, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in State, IO Success Kleisli") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioState[C, V, L, String]("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in State, IO Error Kleisli") {
        val result =
          for {
            a <- CaioState[C, V, L, String]("value", simpleState)
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState12)
      }
      it("Result in IO Error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState))))
            b <- CaioState[C, V, L, String]("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in Error, Error") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
            b <- CaioError[C, V, L, Int](Left(exception2), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result inError, Kleisli") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioError[C, V, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, error Kleisli") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState))
            b <- CaioError[C, V, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in Error, IO Success Kleisli") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Left(exception1), simpleState)
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioError[C, V, L, Int](Left(exception1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in Error, IO Error Kleisli") {
        val result =
          for {
            a <- CaioError[C, V, L, Int](Right(failures1), simpleState)
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in IO Error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState))))
            b <- CaioError[C, V, L, Int](Right(failures1), simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }


      it("Result in success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Right(failures1), simpleState))
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in IO Error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState))))
            b <- CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState))))
            b <- CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Result in IO Success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState12)
      }
      it("Result in IO Success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState12)
      }
      it("Result in IO Error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState))))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }

      it("Result in IO Error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState))))
            b <- CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }
  }

  describe("Preserve under exception") {

    it("Preserve in Kleisli") {
      val result =
        CaioKleisli[C, V, L, String](c => throw exception1)
      getResult(result) shouldBe toResult(Left(exception1), emptyState)
    }

    describe("Under a flat map") {

      it("Preserve in State") {
        val result =
          CaioState[C, V, L, String]("value", simpleState).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[C, V, L, Int](Left(exception1), simpleState).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }

      it("Preserve in Error Failure") {
        val result =
          CaioError[C, V, L, Int](Right(failures1), simpleState).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState)).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState)).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Failure Kleisli") {
        val result =
          CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Right(failures1), simpleState)).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState)))).flatMap(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState)))).flatMap(_ => throw exception2)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
    }

    describe("Under a map") {
      it("Preserve in State") {
        val result =
          CaioState[C, V, L, String]("value", simpleState).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[C, V, L, Int](Left(exception1), simpleState).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in Failure Error") {
        val result =
          CaioError[C, V, L, Int](Right(failures1), simpleState).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => SuccessResult("value", simpleState)).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Left(exception1), simpleState)).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Preserve in error Failure Kleisli") {
        val result =
          CaioKleisli[C, V, L, Int](c => ErrorResult[C, V, L, Int](Right(failures1), simpleState)).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => IOResult(IO.delay(SuccessResult("value", simpleState)))).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Left(exception1), simpleState)))).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Left(exception1), simpleState)
      }
      it("Result in IO Error Failure Kleisli") {
        val result =
          CaioKleisli[C, V, L, String](c => IOResult(IO.delay(ErrorResult[C, V, L, String](Right(failures1), simpleState)))).map(_ => throw exception1)
        getResult(result) shouldBe toResult(Right(failures1), simpleState)
      }
    }

  }*/
}
