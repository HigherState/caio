package caio.std

import caio.{Caio, Failure}
import caio.Event.EventLog
import caio.mtl.Provider
import cats.{Functor, Monad}
import cats.mtl.ApplicativeAsk
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioExtenderTests  extends AsyncFunSpec with Matchers{

  type CaioT[A] = Caio[Unit, Failure, EventLog, A]

  implicit val M:Monad[CaioT] = new CaioMonad[Unit, Failure, EventLog]

  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskString[M[_]:ApplicativeAsk[*[_], String]] {
    def run:M[String] = ApplicativeAsk[M, String].ask
  }

  class AskIntString[M[_]:ApplicativeAsk[*[_], (Int, String)]:Functor] {
    def run:M[(Int, String)] = ApplicativeAsk[M,(Int, String)].ask
  }

  class AskIntBooleanString[M[_]:ApplicativeAsk[*[_], (Int, Boolean, String)]] {
    def run:M[(Int, Boolean, String)] = ApplicativeAsk[M,(Int, Boolean, String)].ask
  }


  class AddAskContext[M[_]](implicit C:Provider[M]) {
    import caio.mtl.Contextual._

    implicit val E = C.apply[Int]

    val service = new AskInt[E.FE]

    def run:M[Int] = E.apply(3)(service.run)
  }

  class AddAskThreeContext[M[_]:Monad](implicit C:Provider[M]) {
    import caio.mtl.Contextual._
    import cats.implicits._

    implicit val E = C.apply[(String,Int)]

    val service1 = new AskInt[E.FE]

    val service2 = new AskString[E.FE]

    val service3 = new AskIntString[E.FE]

    def run(s:String, i:Int): M[(Int, String, (Int, String))] =
      for {
        s1 <- E.apply(s -> i)(service1.run)
        s2 <- E.apply(s -> i)(service2.run)
        s3 <- E.apply(s -> i)(service3.run)
      } yield (s1, s2, s3)
  }

  def runSuccess[A](caio:CaioT[A]):A =
    caio.eval(()).unsafeRunSync()._1.getOrElse(???)

  describe("Provider tests") {
    import CaioProvider._

    it("Should lift value") {
      val ask = new AddAskContext[CaioT]()
      runSuccess(ask.run) shouldBe 3
    }
    it("Should handle recombining context") {
      val ask = new AddAskThreeContext[CaioT]()
      runSuccess(ask.run("value",0)) shouldBe ((0, "value", 0 -> "value"))
    }
  }
}
