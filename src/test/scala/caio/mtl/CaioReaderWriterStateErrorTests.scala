package caio.mtl

import caio._
import caio.std._
import cats.effect.{IO, LiftIO}
import cats.{Monad, MonadError}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, MonadState}
import org.scalatest.{AsyncFunSpec, Matchers}


class CaioReaderWriterStateErrorTests extends AsyncFunSpec with Matchers{
  import Event._
  import Exception._
  import Failure._

  import ContextProjector._

  type L = Vector[Event]
  type C = (String, Int)
  type V = Failure

  type CaioT[A] = Caio[C, V, L, A]

  def run[A](c:C, caio:CaioT[A]):(Either[EoF, A], C, EventLog) =
    caio.unsafeRun(c)


  implicit val MS_ :MonadState[CaioT, C] = new CaioMonadState[C, V, L]
  implicit val AA_ :ApplicativeAsk[CaioT, C] = new CaioApplicativeAsk[C, V, L]
  implicit val AC:ApplicativeCensor[CaioT, EventLog] = new CaioApplicativeCensor[C, V, L] {}
  implicit val ME:MonadError[CaioT, Throwable] = new CaioMonadError[C, V, L] {}
  implicit val M:Monad[CaioT] = new CaioMonad[C, V, L] {}
  implicit val L:LiftIO[CaioT] = new CaioLiftIO[C, V, L] {}

  val MS:MonadState[CaioT, String] =
    implicitly[MonadState[CaioT, String]]

  val AA:ApplicativeAsk[CaioT, Int] =
    implicitly[ApplicativeAsk[CaioT, Int]]

  describe("Setting and retrieving environment") {

    it ("Should get the state") {
      val result =
        for {
          s <- AA.ask
        } yield s
      run("Testing" -> 123, result)._1 shouldBe Right(123)
    }

  }

  describe("Setting and retrieving state") {

    it ("Should get the state") {
      val result =
        for {
          s <- MS.get
        } yield s
      run("Testing" -> 123, result)._1 shouldBe Right("Testing")
    }

    it("Should set the state") {
      val result =
        for {
          _ <- MS.set("Altered")
          s <- MS.get
        } yield s

      run("Testing" -> 123, result)._1 shouldBe Right("Altered")
    }
  }

  describe("Tell of an event") {

    it ("Should provide an event") {
      val result =
        for {
          _ <- AC.tell(Vector(event1))
        } yield "finish"
      run("Testing" -> 123, result)._3 shouldBe Vector(event1)
    }

    it ("Should combine events") {
      val result =
        for {
          _ <- AC.tell(Vector(event1))
          _ <- AC.tell(Vector(event2))
        } yield "finish"
      run("Testing" -> 123, result)._3 shouldBe Vector(event1, event2)
    }
  }


  describe("combination of patterns") {
    it("should return the correct values") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(event1))
          i <- AA.ask
          _ <- AC.tell(Vector(event2))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- AC.tell(Vector(event3))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ((Right(444 -> "new value"), "new value" -> 321, Vector(event1, event2, event3)))
    }

    it("should handle an error value") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(event1))
          i <- AA.ask
          _ <- AC.tell(Vector(event2))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- ME.raiseError[Int](exception1)
          _ <- AC.tell(Vector(event3))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ((Left(Left(exception1)), "value" -> 321, Vector(event1, event2)))
    }

    it("should handle IO") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(event1))
          i <- AA.ask
          _ <- AC.tell(Vector(event2))
          _ <- MS.set(a)
          b <- L.liftIO(IO.delay(123))
          i2 <- AA.ask
          a2 <- MS.get
          _ <- AC.tell(Vector(event3))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ((Right(444 -> "new value"), "new value" -> 321 ,Vector(event1, event2, event3)))
    }

    it("should handle an IO error value") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(event1))
          i <- AA.ask
          _ <- AC.tell(Vector(event2))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- L.liftIO(IO.raiseError[Int](exception1))
          _ <- AC.tell(Vector(event3))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing" -> 321, result) shouldBe ((Left(Left(exception1)), "value" -> 321, Vector(event1, event2)))
    }
  }
}
