package caio.std

import caio._
import cats.effect.ExitCase.{Completed, Error}
import cats.Monad
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioBracketTests extends AsyncFunSpec with Matchers {
  import Event._

  type L = Vector[Event]
  type C = Unit

  type CaioT[A] = Caio[C, L, A]

  def run[A](caio: CaioT[A]): (C, L, Either[Throwable, A]) =
    caio.runContext(()).unsafeRunSync()

  val BC: CaioBracket[C, L]    = new CaioBracket[C, L]()
  implicit val M: Monad[CaioT] = BC

  describe("Simple evaluation and failure") {
    it("Should succeed in simple case") {
      val program = BC.bracketCase(Caio.pure("test")) { a =>
        Caio.pure(a + 2)
      } {
        case (_, Completed) =>
          Caio.unit
        case (_, _)         =>
          fail()
      }
      run(program)._3 shouldBe Right("test2")
    }

    it("Should handle Caio Error") {
      val program = BC.bracketCase(Caio.pure("test")) { _ =>
        Caio.raiseError(Exception.exception1)
      } {
        case (_, Error(ex)) =>
          ex shouldBe Exception.exception1
          Caio.unit
        case (_, _)         =>
          fail()
      }
      run(program)._3 shouldBe Left(Exception.exception1)
    }

    it("Should handle Caio Error in release when use is successful") {
      val program = BC.bracketCase(Caio.pure("test")) { a =>
        Caio.pure(a + 2)
      } {
        case (_, Completed) =>
          Caio.raiseError(Exception.exception1)
        case (_, _)         =>
          fail()
      }
      run(program)._3 shouldBe Left(Exception.exception1)
    }

    it("Should handle Caio Error in release with Error") {
      val program = BC.bracketCase(Caio.pure("test")) { a =>
        Caio.raiseError(Exception.exception1)
      } {
        case (_, Error(_)) =>
          Caio.raiseError(Exception.exception2)
        case (_, _)        =>
          fail()
      }
      run(program)._3 shouldBe Left(Exception.exception1)
    }

  }
  describe("Simple evaluation and failure with Logs") {
    it("Should succeed in simple case with Logs in acquire and use") {
      val program = BC.bracketCase(Caio.tell[L](Vector(Event.event1)) *> Caio.pure("test")) { a =>
        Caio.tell[L](Vector(Event.event2)).flatMap(_ => Caio.pure(a + 2))
      } {
        case (_, Completed) =>
          Caio.unit
        case (_, _)         =>
          fail()
      }
      run(program)._3 shouldBe Right("test2")
      run(program)._2 shouldBe Vector(Event.event1, Event.event2)
    }

    it("Should succeed in simple case with Logs in acquire, use and release") {
      val program = BC.bracketCase(Caio.tell[L](Vector(Event.event1)) *> Caio.pure("test")) { a =>
        Caio.tell[L](Vector(Event.event2)).flatMap(_ => Caio.pure(a + 2))
      } {
        case (_, Completed) =>
          Caio.tell[L](Vector(Event.event3))
        case (_, _)         =>
          fail()
      }
      run(program)._3 shouldBe Right("test2")
      run(program)._2 shouldBe Vector(Event.event1, Event.event2, Event.event3)
    }

    it("Should error in error case with Logs in acquire, use and release") {
      val program = BC.bracketCase(Caio.tell[L](Vector(Event.event1)) *> Caio.pure("test")) { a =>
        Caio.tell[L](Vector(Event.event2)).flatMap(_ => Caio.raiseError(Exception.exception1))
      } {
        case (_, Error(_)) =>
          Caio.tell[L](Vector(Event.event3))
        case (_, _)        =>
          fail()
      }
      run(program)._3 shouldBe Left(Exception.exception1)
      run(program)._2 shouldBe Vector(Event.event1, Event.event2, Event.event3)
    }

    it("Should error in error case with Logs in acquire, use and release exception") {
      val program = BC.bracketCase(Caio.tell[L](Vector(Event.event1)) *> Caio.pure("test")) { a =>
        Caio.tell[L](Vector(Event.event2)) *> Caio.raiseError(Exception.exception1)
      } {
        case (_, Error(_)) =>
          Caio.tell[L](Vector(Event.event3)) *> Caio.raiseError(Exception.exception2)
        case (_, _)        =>
          fail()
      }
      run(program)._3 shouldBe Left(Exception.exception1)
      run(program)._2 shouldBe Vector(Event.event1, Event.event2, Event.event3)
    }

  }
}
