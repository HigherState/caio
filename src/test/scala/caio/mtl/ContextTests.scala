package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}

class ContextTests {

  //Creating internal
  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskString[M[_]:ApplicativeAsk[*[_], String]] {
    def run:M[String] = ApplicativeAsk[M, String].ask
  }

  class AskIntString[M[_]:ApplicativeAsk[*[_], (Int, String)]] {
    def run:M[(Int, String)] = ApplicativeAsk[M,(Int, String)].ask
  }

  class AskIntBooleanString[M[_]:ApplicativeAsk[*[_], (Int, Boolean, String)]] {
    def run:M[(Int, Boolean, String)] = ApplicativeAsk[M,(Int, Boolean, String)].ask
  }


  class StateString[M[_]:MonadState[*[_], String]] {
    def run:M[String] = MonadState[M, String].get
  }

  class StateIntString[M[_]:MonadState[*[_], (Int, String)]] {
    def run:M[(Int, String)] = MonadState[M,(Int, String)].get
  }

  class StateAskIntBooleanString[M[_]:ApplicativeAsk[*[_], (Int, String)]:MonadState[*[_], Boolean]] {
    def run:M[(Int, Boolean, String)] = ???
  }


  class AddAskContext[M[_]](implicit C:Provider[M]) {
    import caio.mtl.Contextual._

    implicit val E = C.apply[Int]

    val service = new AskInt[E.FE]

    def run:M[Int] = E.apply(3)(service.run)
  }

  class ComplexStateString[M[_]:MonadState[*[_], String]] {
    def run:M[String] = MonadState[M,String].get
  }

  class AddStateContext[M[_]:Provider] {
    import Contextual._

    implicit val e = implicitly[Provider[M]].apply[String]

    val service = new ComplexStateString[e.FE]

    def run:M[String] = e.apply("Value")(service.run)
  }

  class Dependency[M[_], MC[_]:Provided[*[_], M, Int]]
    (a:AskInt[MC]){

    val e = implicitly[Provided[MC, M, Int]]

    def run:M[Int] = e.apply(3)(a.run)
  }

  class Dependency2[M[_],
    MC[_]:Provided[*[_], M, Int],
    MC2[_]:Provided[*[_], MC, String]
  ](a:AskIntString[MC2]){

    val e = implicitly[Provided[MC, M, Int]]
    val e2 = implicitly[Provided[MC2, MC, String]]

    def run:M[(Int, String)] =
      e.apply(3)(e2.apply("value")(a.run))
  }

  class AskContext2[M[_]:Provider] {
    import Contextual._

    implicit val e:Provides[M, String] =
      implicitly[Provider[M]].apply[String]

    val nested = new AskContextNested[e.FE]()
  }

  class AskContextNested[M[_]:Extender[*[_], String]:ApplicativeAsk[*[_], String]] {
    import Contextual._

    val string2 = new AskString[M]

    implicit val e = implicitly[Extender[M, String]].apply[Int]

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val string1 = new AskString[e.FE]

  }

  class AskContextNeseted2[M[_]:Extender[*[_], (Int, String)]:ApplicativeAsk[*[_], (Int, String)]] {
    import ContextProjector._
    val string2 = new AskString[M]

    implicit val e = implicitly[Extender[M, (Int, String)]].apply[Boolean]
    //TODO support with ContextProjector
    import Contextual._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val service3 = new AskIntBooleanString[e.FE]

    val string1 = new AskString[e.FE]
  }


  class MixContext2[M[_]:Provider] {
    import Contextual._

    implicit val e:Provides[M, String] =
      implicitly[Provider[M]].apply[String]

    val state1 = new StateString[e.FE]

    val nested = new MixContextNested[e.FE]()
  }

  class MixContextNested[M[_]:Extender[*[_], String]:MonadState[*[_], String]] {
    import Contextual._

    val string2 = new StateString[M]
    //val string4 = new AskString[M]

    implicit val e = implicitly[Extender[M, String]].apply[Int]


    val service = new AskInt[e.FE]

    val service2 = new StateIntString[e.FE]

    val string1 = new StateString[e.FE]

    val string3 = new AskString[e.FE]
  }

  class AddContextMixed2[M[_]:Extender[*[_], (Int, String)]:ApplicativeAsk[*[_], (Int, String)]] {
    import Contextual._

    //val string2 = new AskString[M]
//    val intString = new AskIntString[M]
//    val intString2 = new StateIntString[M]

    implicit val e = implicitly[Extender[M, (Int, String)]].apply[Boolean]

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val service3 = new StateIntString[e.FE]

    val service4 = new StateAskIntBooleanString[e.FE]

    val string1 = new AskString[e.FE]
  }


}
