package caio.std

import caio._
import caio.implicits.StaticImplicits
import cats.effect.{IO, LiftIO, Sync}
import cats.{Applicative, MonadError}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, FunctorTell, MonadState}
import org.scalatest.{AsyncFunSpec, Matchers}


class CaioEvaluationTests extends AsyncFunSpec with Matchers{
  import Event._
  import Exception._
  import Failure._

  type L = Vector[Event]
  type C = (String, Int)
  type V = Failure

  type CaioT[A] = Caio[C, V, L, A]

  def run[A](c:C, caio:CaioT[A]): (C, L, Either[EoF, A]) = {
    caio.unsafeRunContext(c)
  }

  import caio.mtl.ContextProjector._
  val implicits = new StaticImplicits[C, V, L]()
  import implicits._
  import cats.Monad.ops._

  describe("Setting and retrieving environment") {

    it ("Should get the state") {
      val result =
        for {
          s <- ApplicativeAsk.ask[CaioT, Int]
        } yield s
      run("Testing" -> 123, result)._3 shouldBe Right(123)
    }

  }

  describe("Setting and retrieving state") {

    it ("Should get the state") {
      val result =
        for {
          s <- MonadState.get[CaioT, String]
        } yield s
      run("Testing" -> 123, result)._3 shouldBe Right("Testing")
    }

    it("Should set the state") {
      val result =
        for {
          _ <- MonadState.set[CaioT, String]("Altered")
          s <- MonadState.get[CaioT, String]
        } yield s

      run("Testing" -> 123, result)._3 shouldBe Right("Altered")
    }
  }

  describe("Tell of an event") {

    it ("Should provide an event") {
      val result =
        for {
          _ <- FunctorTell.tell[CaioT, L](Vector(event1))
        } yield "finish"
      run("Testing" -> 123, result)._2 shouldBe Vector(event1)
    }

    it ("Should combine events") {
      val result =
        for {
          _ <- FunctorTell.tell[CaioT, L](Vector(event1))
          _ <- FunctorTell.tell[CaioT, L](Vector(event2))
        } yield "finish"
      run("Testing" -> 123, result)._2 shouldBe Vector(event1, event2)
    }
  }

  describe("Censor event log") {
    it ("Should transform EventLog") {
      val result =
        ApplicativeCensor[CaioT, L]
          .censor(FunctorTell.tell[CaioT, L](Vector(event1, event2)))(_.reverse)
      run("Testing" -> 123, result)._2 shouldBe Vector(event2, event1)
    }
  }
  describe("throwing an exception") {
    it("Should throw exception if pure") {
      intercept[Exception](IO.pure(throw Exception.exception1)) shouldBe Exception.exception1
      intercept[Exception](Applicative[CaioT].pure(throw Exception.exception1)) shouldBe Exception.exception1
    }
    it("Should capture exception if delay") {
      run("1" -> 1, Sync[CaioT].delay(throw Exception.exception1)) shouldBe ("1" -> 1, Vector.empty, Left(Left(Exception.exception1)))
    }
    it("Should capture exception if flatMap") {
      val iof =
        IO.pure("a")
          .flatMap{a => throw Exception.exception1}
      val f =
        Applicative[CaioT].pure("a")
          .flatMap{a => throw Exception.exception1}
      run("1" -> 1, f) shouldBe ("1" -> 1, Vector.empty, Left(Left(Exception.exception1)))
    }
    it("Should capture exception if map") {
      pending
    }
  }


  describe("combination of patterns") {
    it("should return the correct values") {
      val result =
        for {
          a <- Applicative[CaioT].pure("value")
          _ <- FunctorTell[CaioT, L].tell(Vector(event1))
          _ <- ApplicativeAsk[CaioT, Int].ask
          _ <- FunctorTell[CaioT, L].tell(Vector(event2))
          _ <- MonadState[CaioT, String].set(a)
          b <- Applicative[CaioT].pure(123)
          _ <- ApplicativeAsk[CaioT, Int].ask
          a2 <- MonadState[CaioT, String].get
          _ <- FunctorTell[CaioT, L].tell(Vector(event3))
          _ <- MonadState[CaioT, String].set("new " + a2)
          i3 <- ApplicativeAsk[CaioT, Int].ask
          a3 <- MonadState[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe (("new value" -> 321, Vector(event1, event2, event3), Right(444 -> "new value")))
    }

    it("should handle an error value") {
      val result =
        for {
          a <- Applicative[CaioT].pure("value")
          _ <- FunctorTell[CaioT, L].tell(Vector(event1))
          i <- ApplicativeAsk[CaioT, Int].ask
          _ <- FunctorTell[CaioT, L].tell(Vector(event2))
          _ <-  MonadState[CaioT, String].set(a)
          b <- Applicative[CaioT].pure(123)
          i2 <- ApplicativeAsk[CaioT, Int].ask
          a2 <-  MonadState[CaioT, String].get
          _ <- MonadError[CaioT, Throwable].raiseError[Int](exception1)
          _ <-FunctorTell[CaioT, L].tell(Vector(event3))
          _ <-  MonadState[CaioT, String].set("new " + a2)
          i3 <- ApplicativeAsk[CaioT, Int].ask
          a3 <-  MonadState[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ("value" -> 321, Vector(event1, event2), (Left(Left(exception1))))
    }

    it("should handle IO") {
      val result =
        for {
          a <- Applicative[CaioT].pure("value")
          _ <- FunctorTell[CaioT, L].tell(Vector(event1))
          i <- ApplicativeAsk[CaioT, Int].ask
          _ <- FunctorTell[CaioT, L].tell(Vector(event2))
          _ <- MonadState[CaioT, String].set(a)
          b <- LiftIO[CaioT].liftIO(IO.delay(123))
          i2 <- ApplicativeAsk[CaioT, Int].ask
          a2 <- MonadState[CaioT, String].get
          _ <- FunctorTell[CaioT, L].tell(Vector(event3))
          _ <- MonadState[CaioT, String].set("new " + a2)
          i3 <- ApplicativeAsk[CaioT, Int].ask
          a3 <- MonadState[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe (("new value" -> 321 ,Vector(event1, event2, event3), Right(444 -> "new value")))
    }

    it("should handle an IO error value") {
      val result =
        for {
          a <- Applicative[CaioT].pure("value")
          _ <- FunctorTell[CaioT, L].tell(Vector(event1))
          i <- ApplicativeAsk[CaioT, Int].ask
          _ <- FunctorTell[CaioT, L].tell(Vector(event2))
          _ <- MonadState[CaioT, String].set(a)
          b <- Applicative[CaioT].pure(123)
          i2 <- ApplicativeAsk[CaioT, Int].ask
          a2 <- MonadState[CaioT, String].get
          _ <- LiftIO[CaioT].liftIO(IO.raiseError[Int](exception1))
          _ <- FunctorTell[CaioT, L].tell(Vector(event3))
          _ <- MonadState[CaioT, String].set("new " + a2)
          i3 <- ApplicativeAsk[CaioT, Int].ask
          a3 <- MonadState[CaioT, String].get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe (("value" -> 321, Vector(event1, event2), Left(Left(exception1))))
    }
  }
}
