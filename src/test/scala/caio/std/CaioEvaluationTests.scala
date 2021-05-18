package caio.std

import caio._
import caio.implicits.StaticImplicits
import caio.mtl.InvariantAsk
import cats.effect.{IO, LiftIO}
import cats.{Applicative, MonadError, Monoid}
import cats.mtl.{Censor, Stateful, Tell}
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioEvaluationTests extends AsyncFunSpec with Matchers {
  import caio.mtl.ContextProjector._
  import Event._
  import Exception._
  import Failure._

  type L = Vector[Event]
  type C = (String, Int)
  type V = Failure

  type CaioT[A] = Caio[C, V, L, A]

  def run[A](c: C, caio: CaioT[A]): (C, L, Either[EoF, A]) =
    caio.runContext(c).unsafeRunSync()

  val implicits = new StaticImplicits[C, V, L] {
    implicit protected def ML: Monoid[L] = EventMonoid
  }

  import implicits._

  describe("Setting and retrieving environment") {
    it("Should get the state") {
      val result = InvariantAsk.ask[CaioT, Int]
      run("Testing" -> 123, result)._3 shouldBe Right(123)
    }
  }

  describe("Setting and retrieving state") {
    it("Should get the state") {
      val result = Stateful.get[CaioT, String]
      run("Testing" -> 123, result)._3 shouldBe Right("Testing")
    }

    it("Should set the state") {
      val result =
        for {
          _ <- Stateful.set[CaioT, String]("Altered")
          s <- Stateful.get[CaioT, String]
        } yield s

      run("Testing" -> 123, result)._3 shouldBe Right("Altered")
    }
  }

  describe("Tell of an event") {

    it("Should provide an event") {
      val result = Tell.tell[CaioT, L](Vector(event1)).as("finish")
      run("Testing" -> 123, result)._2 shouldBe Vector(event1)
    }

    it("Should combine events") {
      val result =
        for {
          _ <- Tell.tell[CaioT, L](Vector(event1))
          _ <- Tell.tell[CaioT, L](Vector(event2))
        } yield "finish"
      run("Testing" -> 123, result)._2 shouldBe Vector(event1, event2)
    }
  }

  describe("Censor event log") {
    it("Should transform EventLog") {
      val result =
        Censor[CaioT, L]
          .censor(Tell.tell[CaioT, L](Vector(event1, event2)))(_.reverse)
      run("Testing" -> 123, result)._2 shouldBe Vector(event2, event1)
    }
  }
  describe("throwing an exception") {
    it("Should throw exception if pure") {
      intercept[Exception](IO.pure[Unit](throw Exception.exception1)) shouldBe Exception.exception1
      intercept[Exception](Caio.pure[Unit](throw Exception.exception1)) shouldBe Exception.exception1
    }
    it("Should capture exception if delay") {
      run("1" -> 1, Caio(throw Exception.exception1)) shouldBe ("1" -> 1, Vector.empty, Left(Left(Exception.exception1)))
    }
    it("Should capture exception if flatMap") {
      val f =
        Applicative[CaioT]
          .pure("a")
          .flatMap(_ => throw Exception.exception1)
      run("1" -> 1, f) shouldBe ("1" -> 1, Vector.empty, Left(Left(Exception.exception1)))
    }
    it("Should capture exception if map") {
      val f =
        Applicative[CaioT]
          .pure("a")
          .map(_ => throw Exception.exception1)
      run("1" -> 1, f) shouldBe ("1" -> 1, Vector.empty, Left(Left(Exception.exception1)))
    }
  }

  describe("combination of patterns") {
    it("should return the correct values") {
      val result =
        for {
          a  <- Applicative[CaioT].pure("value")
          _  <- Tell[CaioT, L].tell(Vector(event1))
          _  <- InvariantAsk.ask[CaioT, Int]
          _  <- Tell[CaioT, L].tell(Vector(event2))
          _  <- Stateful[CaioT, String].set(a)
          b  <- Applicative[CaioT].pure(123)
          _  <- InvariantAsk.ask[CaioT, Int]
          a2 <- Stateful[CaioT, String].get
          _  <- Tell[CaioT, L].tell(Vector(event3))
          _  <- Stateful[CaioT, String].set("new " + a2)
          i3 <- InvariantAsk.ask[CaioT, Int]
          a3 <- Stateful[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing"   -> 321, result) shouldBe (
        (
          "new value" -> 321,
          Vector(event1, event2, event3),
          Right(444 -> "new value")
        )
      )
    }

    it("should handle an error value") {
      val result =
        for {
          a  <- Applicative[CaioT].pure("value")
          _  <- Tell[CaioT, L].tell(Vector(event1))
          _  <- InvariantAsk.ask[CaioT, Int]
          _  <- Tell[CaioT, L].tell(Vector(event2))
          _  <- Stateful[CaioT, String].set(a)
          b  <- Applicative[CaioT].pure(123)
          _  <- InvariantAsk.ask[CaioT, Int]
          a2 <- Stateful[CaioT, String].get
          _  <- MonadError[CaioT, Throwable].raiseError[Int](exception1)
          _  <- Tell[CaioT, L].tell(Vector(event3))
          _  <- Stateful[CaioT, String].set("new " + a2)
          i3 <- InvariantAsk.ask[CaioT, Int]
          a3 <- Stateful[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ("value" -> 321, Vector(event1, event2), (Left(Left(exception1))))
    }

    it("should handle IO") {
      val result =
        for {
          a  <- Applicative[CaioT].pure("value")
          _  <- Tell[CaioT, L].tell(Vector(event1))
          _  <- InvariantAsk.ask[CaioT, Int]
          _  <- Tell[CaioT, L].tell(Vector(event2))
          _  <- Stateful[CaioT, String].set(a)
          b  <- LiftIO[CaioT].liftIO(IO.delay(123))
          _  <- InvariantAsk.ask[CaioT, Int]
          a2 <- Stateful[CaioT, String].get
          _  <- Tell[CaioT, L].tell(Vector(event3))
          _  <- Stateful[CaioT, String].set("new " + a2)
          i3 <- InvariantAsk.ask[CaioT, Int]
          a3 <- Stateful[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing"   -> 321, result) shouldBe (
        (
          "new value" -> 321,
          Vector(event1, event2, event3),
          Right(444 -> "new value")
        )
      )
    }

    it("should handle an IO error value") {
      val result =
        for {
          a  <- Applicative[CaioT].pure("value")
          _  <- Tell[CaioT, L].tell(Vector(event1))
          _  <- InvariantAsk.ask[CaioT, Int]
          _  <- Tell[CaioT, L].tell(Vector(event2))
          _  <- Stateful[CaioT, String].set(a)
          b  <- Applicative[CaioT].pure(123)
          _  <- InvariantAsk.ask[CaioT, Int]
          a2 <- Stateful[CaioT, String].get
          _  <- LiftIO[CaioT].liftIO(IO.raiseError[Int](exception1))
          _  <- Tell[CaioT, L].tell(Vector(event3))
          _  <- Stateful[CaioT, String].set("new " + a2)
          i3 <- InvariantAsk.ask[CaioT, Int]
          a3 <- Stateful[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe (("value" -> 321, Vector(event1, event2), Left(Left(exception1))))
    }
  }
}
