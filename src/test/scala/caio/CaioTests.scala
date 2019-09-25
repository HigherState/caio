package caio

import cats.effect.IO
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioTests extends AsyncFunSpec with Matchers{
  import Events._
  import Exceptions._

  val simpleState:State = State(Vector(one, two)) + (Context.empty + "bob" + 123)
  val simpleState2:State = State(Vector(three)) + (Context.empty + 413 + false)
  val combineState:State = State(Vector(one, two, three)) + (Context.empty + "bob" + 413 + false)

  //including context should effect tests
  def getResult[A](c:Caio[A]):(Either[Throwable, A], State) =
    c.toResult(Context.empty + 22L).toIO.unsafeRunSync() match {
      case SuccessResult(a, state) =>
        Right(a) -> state
      case ErrorResult(ex, state) =>
        Left(ex) -> state
    }

  def toResult[A](t:Throwable, s:State):(Either[Throwable, A], State) =
    Left(t) -> s

  def toResult[A](a:A, s:State):(Either[Throwable, A], State) =
    Right(a) -> s


  describe("Preserving of state and result in For comprehension") {

    describe("A single operation") {
      it("Empty state in pure") {
        val result =
          for {
            a <- CaioPure("value")
          } yield a
        getResult(result) shouldBe toResult("value", State.empty)
      }
      it("Result inState") {
        val result =
          for {
            a <- CaioState("value", simpleState)
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }
      it("Result inError") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
          } yield a
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
          } yield a
        getResult(result) shouldBe toResult(testEx, simpleState)
      }

      it("Result in IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
          } yield a
        getResult(result) shouldBe toResult("value", simpleState)
      }

      it("Result in IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
          } yield a
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
    }

    describe("Paired Operations") {
      it("Result inPure,Pure") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioPure("value2")
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", State.empty)
      }
      it("Result inPure,State") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioState("value2", simpleState)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result inState Pure") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioPure("value2")
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result inPure, Error") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioError[Int](testEx, simpleState)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result inError, Pure") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioPure("value")
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in Pure, success Kleisli") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioKleisli(c => SuccessResult("value2", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result in success Kleisli, Pure") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioPure("value2")
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result in Pure, error Kleisli") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in error Kleisli, Pure") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioPure("value")
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in Pure, IO Success Kleisli") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result in IO Success Kleisli, Pure") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioPure("value2")
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", simpleState)
      }
      it("Result in Pure, IO Error Kleisli") {
        val result =
          for {
            a <- CaioPure("value")
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Error Kleisli, Pure") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioPure("value")
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }


      it("Result in State,State") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioState("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in State, Error") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioError[Int](testEx, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in Error, State") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioState("value", simpleState)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in State, success Kleisli") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioKleisli(c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioState("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in State, error Kleisli") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioState("value", simpleState)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in State, IO Success Kleisli") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in IO Success Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioState("value2", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in State, IO Error Kleisli") {
        val result =
          for {
            a <- CaioState("value", simpleState)
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in IO Error Kleisli, State") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <-  CaioState("value", simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }


      it("Result in Error, Error") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioError[Int](testEx2, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result inError, Kleisli") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioKleisli(c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioError[Int](testEx, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in Error, error Kleisli") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioKleisli(c => ErrorResult[Int](testEx2, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioError[Int](testEx2, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in Error, IO Success Kleisli") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Success Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioError[Int](testEx, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in Error, IO Error Kleisli") {
        val result =
          for {
            a <- CaioError[Int](testEx, simpleState)
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Error Kleisli, Error") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioError[Int](testEx2, simpleState2)
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }


      it("Result in success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioKleisli(c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioKleisli(c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in IO Success Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli(c => SuccessResult("value2", simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => SuccessResult("value", simpleState))
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in IO Error Kleisli, success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioKleisli(c => SuccessResult("value", simpleState))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }


      it("Result in error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioKleisli(c => ErrorResult[Int](testEx2, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Success Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => ErrorResult[Int](testEx, simpleState))
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx2, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Error Kleisli, error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioKleisli(c => ErrorResult[Int](testEx2, simpleState2))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }

      it("Result in IO Success Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value2", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult("value" -> "value2", combineState)
      }
      it("Result in IO Success Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState))))
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, combineState)
      }
      it("Result in IO Error Kleisli, IO Success Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }

      it("Result in IO Error Kleisli, IO Error Kleisli") {
        val result =
          for {
            a <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState))))
            b <- CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx2, simpleState2))))
          } yield a -> b
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
    }
  }

  describe("Preserve under exception") {

    it("Preserve in Kleisli") {
      val result =
        CaioKleisli(c => throw testEx)
      getResult(result) shouldBe toResult(testEx, State.empty)
    }

    describe("Under a flat map") {
      it("Preserve in Pure") {
        val result =
          CaioPure("value").flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, State.empty)
      }
      it("Preserve in State") {
        val result =
          CaioState("value", simpleState).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[Int](testEx, simpleState).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli(c => SuccessResult("value", simpleState)).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli(c => ErrorResult[Int](testEx, simpleState)).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState)))).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState)))).flatMap(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
    }

    describe("Under a map") {
      it("Preserve in Pure") {
        val result =
          CaioPure("value").map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, State.empty)
      }
      it("Preserve in State") {
        val result =
          CaioState("value", simpleState).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Preserve in Error") {
        val result =
          CaioError[Int](testEx, simpleState).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }

      it("Preserve in success Kleisli") {
        val result =
          CaioKleisli(c => SuccessResult("value", simpleState)).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Preserve in error Kleisli") {
        val result =
          CaioKleisli(c => ErrorResult[Int](testEx, simpleState)).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Success Kleisli") {
        val result =
          CaioKleisli(c => IOResult(IO.delay(SuccessResult("value", simpleState)))).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
      it("Result in IO Error Kleisli") {
        val result =
          CaioKleisli(c => IOResult(IO.delay(ErrorResult[String](testEx, simpleState)))).map(_ => throw testEx)
        getResult(result) shouldBe toResult(testEx, simpleState)
      }
    }

  }
}
