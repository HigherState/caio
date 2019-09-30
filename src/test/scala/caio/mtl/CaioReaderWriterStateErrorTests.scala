package caio.mtl

import caio._
import caio.std.{CaioApplicativeAsk, CaioApplicativeCensor, CaioLiftIO, CaioMonad, CaioMonadError, CaioMonadState}
import cats.effect.{IO, LiftIO}
import cats.{Monad, MonadError}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, MonadState}
import org.scalatest.{AsyncFunSpec, Matchers}

import scala.reflect.ClassTag

class CaioReaderWriterStateErrorTests extends AsyncFunSpec with Matchers{
  import Events._
  import Exceptions._

  def run[S:ClassTag, A](s:S, caio:Caio[A]):(Either[Throwable, A],EventLog) =
    caio.runAsync(Arguments(s)).unsafeRunSync()

  def run[S:ClassTag, S2:ClassTag, A](s:S, s2:S2, caio:Caio[A]):(Either[Throwable, A],EventLog) =
    caio.runAsync(Arguments(s) + s2).unsafeRunSync()

  implicit val MS:MonadState[Caio, String] = new CaioMonadState[String]
  implicit val AC:ApplicativeCensor[Caio, EventLog] = new CaioApplicativeCensor {}
  implicit val AA:ApplicativeAsk[Caio, Int] = new CaioApplicativeAsk[Int]
  implicit val ME:MonadError[Caio, Throwable] = new CaioMonadError {}
  implicit val M:Monad[Caio] = new CaioMonad {}
  implicit val L:LiftIO[Caio] = new CaioLiftIO {}

  describe("Setting and retrieving state") {

    it ("Should get the state") {
      val result =
        for {
          s <- MS.get
        } yield s
      run("Testing", result)._1 shouldBe Right("Testing")
    }

    it("Should set the state") {
      val result =
        for {
          _ <- MS.set("Altered")
          s <- MS.get
        } yield s
      run("Testing", result)._1 shouldBe Right("Altered")
    }
  }

  describe("Tell of an event") {

    it ("Should provide an event") {
      val result =
        for {
          _ <- AC.tell(Vector(one))
        } yield "finish"
      run("Testing", result)._2 shouldBe Vector(one)
    }

    it ("Should combine events") {
      val result =
        for {
          _ <- AC.tell(Vector(one))
          _ <- AC.tell(Vector(two))
        } yield "finish"
      run("Testing", result)._2 shouldBe Vector(one, two)
    }
  }


  describe("combination of patterns") {
    it("should return the correct values") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(one))
          i <- AA.ask
          _ <- AC.tell(Vector(two))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- AC.tell(Vector(three))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing", 321, result) shouldBe (Right(444 -> "new value") -> Vector(one, two, three))
    }

    it("should handle an error value") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(one))
          i <- AA.ask
          _ <- AC.tell(Vector(two))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- ME.raiseError[Int](testEx)
          _ <- AC.tell(Vector(three))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing", 321, result) shouldBe (Left(testEx) -> Vector(one, two))
    }

    it("should handle IO") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(one))
          i <- AA.ask
          _ <- AC.tell(Vector(two))
          _ <- MS.set(a)
          b <- L.liftIO(IO.delay(123))
          i2 <- AA.ask
          a2 <- MS.get
          _ <- AC.tell(Vector(three))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing", 321, result) shouldBe (Right(444 -> "new value") -> Vector(one, two, three))
    }

    it("should handle an IO error value") {
      val result =
        for {
          a <- M.pure("value")
          _ <- AC.tell(Vector(one))
          i <- AA.ask
          _ <- AC.tell(Vector(two))
          _ <- MS.set(a)
          b <- M.pure(123)
          i2 <- AA.ask
          a2 <- MS.get
          _ <- L.liftIO(IO.raiseError[Int](testEx))
          _ <- AC.tell(Vector(three))
          _ <- MS.set("new " + a2)
          i3 <- AA.ask
          a3 <- MS.get
        } yield (i3 + b) -> a3
      run("Testing", 321, result) shouldBe (Left(testEx) -> Vector(one, two))
    }
  }
}
