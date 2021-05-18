package caio.mtl

import cats.mtl.Stateful

class ContextTests {

  class AskInt[M[_]: InvariantAsk[*[_], Int]] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskString[M[_]: InvariantAsk[*[_], String]] {
    def run: M[String] = InvariantAsk[M, String].ask
  }

  class AskIntString[M[_]: InvariantAsk[*[_], (Int, String)]] {
    def run: M[(Int, String)] = InvariantAsk[M, (Int, String)].ask
  }

  class AskIntBoolean[M[_]: InvariantAsk[*[_], (Int, Boolean)]] {
    def run: M[(Int, Boolean)] = InvariantAsk[M, (Int, Boolean)].ask
  }

  class AskIntBooleanString[M[_]: InvariantAsk[*[_], (Int, Boolean, String)]] {
    def run: M[(Int, Boolean, String)] = InvariantAsk[M, (Int, Boolean, String)].ask
  }

  class StateString[M[_]: Stateful[*[_], String]] {
    def run: M[String] = Stateful[M, String].get
  }

  class StateIntString[M[_]: Stateful[*[_], (Int, String)]] {
    def run: M[(Int, String)] = Stateful[M, (Int, String)].get
  }

  class StateAskIntBooleanString[M[_]: InvariantAsk[*[_], (Int, String)]: Stateful[*[_], Boolean]] {
    def run: M[(Int, Boolean, String)] = ???
  }

  class AddAskContext[M[_]](implicit C: Provider[M]) {

    val E = C.apply[Int]
    import E._

    val service = new AskInt[E.FE]

    def run: M[Int] = E.apply(3)(service.run)
  }

  class ComplexStateString[M[_]: Stateful[*[_], String]] {
    def run: M[String] = Stateful[M, String].get
  }

  class AddStateContext[M[_]: Provider] {

    val E = implicitly[Provider[M]].apply[String]
    import E._

    val service = new ComplexStateString[E.FE]

    def run: M[String] = E.apply("Value")(service.run)
  }

  class Dependency[M[_], MC[_]: Provided[*[_], M, Int]](a: AskInt[MC]) {

    val e = implicitly[Provided[MC, M, Int]]

    def run: M[Int] = e.apply(3)(a.run)
  }

  class Dependency2[M[_], MC[_]: Provided[*[_], M, Int], MC2[_]: Provided[*[_], MC, String]](a: AskIntString[MC2]) {

    val e  = implicitly[Provided[MC, M, Int]]
    val e2 = implicitly[Provided[MC2, MC, String]]

    def run: M[(Int, String)] =
      e.apply(3)(e2.apply("value")(a.run))
  }

  class AskContext2[M[_]: Provider] {

    val e: Provides[M, String] =
      implicitly[Provider[M]].apply[String]
    import e._

    val nested = new AskContextNested[e.FE]()
  }

  class AskContextNested[M[_]: Extender[*[_], String]: InvariantAsk[*[_], String]] {

    val string2 = new AskString[M]

    val e = implicitly[Extender[M, String]].apply[Int]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val string1 = new AskString[e.FE]

  }

  class AskContextNested2[M[_]: Extender[*[_], (Int, String)]] {
    import ContextProjector._

    val string2 = new AskString[M]

    val e = implicitly[Extender[M, (Int, String)]].apply[Boolean]
    //TODO support with ContextProjector
    import e._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val service3 = new AskIntBooleanString[e.FE]

    val string1 = new AskString[e.FE]
  }

  class MixContext2[M[_]: Provider] {

    val e: Provides[M, String] =
      implicitly[Provider[M]].apply[String]

    import e._

    val state1 = new StateString[e.FE]

    val nested = new MixContextNested[e.FE]()
  }

  class MixContextNested[M[_]: Extender[*[_], String]: Stateful[*[_], String]] {
    val string2 = new StateString[M]

    val string4 = {
      import ContextCombinator.{combinatorAsk, stateCombinator}
      new AskString[M]
    }

    val e = implicitly[Extender[M, String]].apply[Int]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new StateIntString[e.FE]

    val string1 = new StateString[e.FE]

    val string3 = new AskString[e.FE]
  }

  class AddContextMixed2[M[_]: Extender[*[_], (Int, String)]] {

    val (string2, intString, intString2) = {
      import ContextProjector._
      (new AskString[M], new AskIntString[M], new StateIntString[M])
    }

    val e = implicitly[Extender[M, (Int, String)]].apply[Boolean]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val service3 = new StateIntString[e.FE]

    val service4 = new StateAskIntBooleanString[e.FE]

    val string1 = new AskString[e.FE]
  }

  class DoubleExtended[M[_]: Extender[*[_], Int]] {

    //Cannot have 2 Extends implicits in scope at the same time
    val ES: Extends[M, Int, String] = implicitly[Extender[M, Int]].apply[String]
    val service1: AskIntString[ES.FE] = {
      import ES._
      new AskIntString[ES.FE]
    }

    val EB: Extends[M, Int, Boolean] = implicitly[Extender[M, Int]].apply[Boolean]
    val service2 = {
      import EB._
      new AskIntBoolean[EB.FE]
    }

    def run(): (M[(Int, String)], M[(Int, Boolean)]) =
      ES.apply("String")(service1.run) ->
        EB.apply(false)(service2.run)
  }

  class DoubleExtendedHierarchical[M[_]: Extender[*[_], Int]] {

    //Cannot have 2 Extends implicits in scope at the same time
    val ES = implicitly[Extender[M, Int]].apply[Boolean]
    val service1: AskIntBoolean[ES.FE] = {
      import ES._
      new AskIntBoolean[ES.FE]
    }

    val EB = ES.extender.apply[String]
    val service2 = {
      import EB._
      new AskIntBooleanString[EB.FE]
    }

    def run() =
      ES.apply(false)(service1.run) ->
        ES.apply(false)(EB.apply("bob")(service2.run))
  }
}
