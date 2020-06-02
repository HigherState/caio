package caio.std

import caio.{Caio, Failure}
import caio.Event.EventLog
import caio.mtl.{ContextProjector, Extender, Provider}
import cats.effect.Effect
import cats.{Functor, Monad}
import cats.mtl.ApplicativeAsk
import org.scalatest.{AsyncFunSpec, Matchers}


class CaioExtenderTests  extends AsyncFunSpec with Matchers{
  import cats.implicits._
  type CaioT[A] = Caio[Unit, Failure, EventLog, A]

  implicit val caioMonad:Monad[CaioT] = new CaioMonad[Unit, Failure, EventLog]

  val effect:Effect[CaioT] = new CaioEffect[Unit, Failure, EventLog](())()()()

  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskAtomic1[M[_]:ApplicativeAsk[*[_], Atomic1]] {
    def run:M[Atomic1] = ApplicativeAsk[M, Atomic1].ask
  }
  class AskAtomic2[M[_]:ApplicativeAsk[*[_], Atomic2]] {
    def run:M[Atomic2] = ApplicativeAsk[M, Atomic2].ask
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

    val E = C.apply[Int]
    import E._

    val service = new AskInt[E.FE]

    def run:M[Int] = E.apply(3)(service.run)
  }

  class AddAskThreeContext[M[_]:Monad](implicit C:Provider[M]) {
    import cats.implicits._

    val E = C.apply[(String,Int)]
    import E._

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
    effect.toIO(caio).unsafeRunSync()

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

  class AddAskThreeExtended[M[_]:Monad:Extender[*[_], (String, Boolean)]] {

    val E = implicitly[Extender[M, (String, Boolean)]].apply[Int]
    import E._

    val service1 = new AskInt[E.FE]

    val service2 = new AskString[E.FE]

    val service3 = new AskIntString[E.FE]

    val service4 = new AskIntBooleanString[E.FE]

    def run(i:Int): M[(Int, String, (Int, String), (Int, Boolean, String))] =
      for {
        s1 <- E.apply(i)(service1.run)
        s2 <- E.apply(i)(service2.run)
        s3 <- E.apply(i)(service3.run)
        s4 <- E.apply(i)(service4.run)
      } yield (s1, s2, s3, s4)
  }

  class NestedContext[M[_]:Monad:Provider] {
    val E = implicitly[Provider[M]].apply[(String, Boolean)]
    import E._

    val service1 = new AskString[E.FE]
    val service2 = new AddAskThreeExtended[E.FE]

    def run(s:String, b:Boolean, i:Int): M[(String, (Int, String, (Int, String), (Int, Boolean, String)))] =
      for {
        s1 <- E.apply(s -> b)(service1.run)
        s2 <- E.apply(s -> b)(service2.run(i))
      } yield (s1, s2)

  }

  describe("Extender tests") {
    import CaioProvider._

    it("Should lift value") {
      val nestedContext = new NestedContext[CaioT]()
      runSuccess(nestedContext.run("test", false, 6)) shouldBe (("test",(6,"test",(6,"test"), (6, false, "test"))))
    }
  }

  class DoubleNestedContext[M[_]:Monad:Provider] {

    val E = implicitly[Provider[M]].apply[(String, Boolean)]
    import E._

    val service1 = new AskString[E.FE]
    val service2 = new ExtenderStringBoolean[E.FE]

    def run(s:String, b:Boolean, ci:Int, a1:Atomic1, a2:Atomic2):
      M[(String,
        (String,
          ((Int, String), (Int, Boolean, String), Atomic1),
          ((Int, String), (Int, Boolean, String), Atomic2),
          Int)
        )] =
      for {
        s1 <- E.apply(s -> b)(service1.run)
        s2 <- E.apply(s -> b)(service2.run(ci, a1, a2))
      } yield (s1, s2)
  }

  class ExtenderStringBoolean[M[_]:Monad:Extender[*[_], (String, Boolean)]] {

    val service0 = {
      import ContextProjector._
      new AskString[M]
    }

    val E = implicitly[Extender[M, (String, Boolean)]].apply[Int]
    import E._

    val service1 = new ExtenderIntString[E.FE]
    val service2 = new ExtenderIntBoolean[E.FE]
    val service3 = new AskInt[E.FE]

    def run(i:Int, a1:Atomic1, a2:Atomic2): M[
      (String,
        ((Int, String), (Int, Boolean, String), Atomic1),
        ((Int, String), (Int, Boolean, String), Atomic2),
      Int)] =
      for {
        s1 <- service0.run
        s2 <- E.apply(i)(service1.run(a1))
        s3 <- E.apply(i)(service2.run(a2))
        s4 <- E.apply(i)(service3.run)
      } yield (s1, s2, s3, s4)

  }

  class ExtenderIntString[M[_]:Monad:Extender[*[_], (Int, String, Boolean)]] {

    val E = implicitly[Extender[M, (Int, String, Boolean)]].apply[Atomic1]
    import E._

    val service1 = new AskIntString[E.FE]
    val service2 = new AskIntBooleanString[E.FE]
    val service3 = new AskAtomic1[E.FE]

    def run(a:Atomic1): M[((Int, String), (Int, Boolean, String), Atomic1)] =
      for {
        s1 <- E.apply(a)(service1.run)
        s2 <- E.apply(a)(service2.run)
        s3 <- E.apply(a)(service3.run)
      } yield (s1, s2, s3)
  }

  class ExtenderIntBoolean[M[_]:Monad:Extender[*[_], (Int, Boolean, String)]] {

    val E = implicitly[Extender[M, (Int, Boolean, String)]].apply[Atomic2]
    import E._

    val service1 = new AskIntString[E.FE]
    val service2 = new AskIntBooleanString[E.FE]
    val service3 = new AskAtomic2[E.FE]

    def run(a:Atomic2): M[((Int, String), (Int, Boolean, String), Atomic2)] =
      for {
        s1 <- E.apply(a)(service1.run)
        s2 <- E.apply(a)(service2.run)
        s3 <- E.apply(a)(service3.run)
      } yield (s1, s2, s3)
  }

  describe("Double extended nested tests") {
    import CaioProvider._
    //It will not replace values of the same type.
    it("Should lift value") {
      val doubleNestedTest = new DoubleNestedContext[CaioT]
      val a1 = new Atomic1("a1")
      val a2 = new Atomic2("a2")
      val toEval = doubleNestedTest.run("test", false, 1, a1, a2)
      runSuccess(toEval) shouldBe (
        ("test",("test",((1,"test"),(1,false,"test"),a1),((1,"test"),(1,false,"test"),a2),1))
      )
    }
  }

}
