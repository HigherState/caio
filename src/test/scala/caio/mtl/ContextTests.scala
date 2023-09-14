package caio.mtl

import cats.mtl.Stateful

class ContextTests {
  type InvariantAskInt[F[_]]        = InvariantAsk[F, Int]
  type InvariantAskString[F[_]]     = InvariantAsk[F, String]
  type InvariantAskIntString[F[_]]  = InvariantAsk[F, (Int, String)]
  type InvariantAskIntBool[F[_]]    = InvariantAsk[F, (Int, Boolean)]
  type InvariantAskIntBoolStr[F[_]] = InvariantAsk[F, (Int, Boolean, String)]


  type StatefulInt[F[_]]       = Stateful[F, Int]
  type StatefulString[F[_]]    = Stateful[F, String]
  type StatefulBoolean[F[_]]   = Stateful[F, Boolean]
  type StatefulIntString[F[_]] = Stateful[F, (Int, String)]


  type ExtenderInt[F[_]]    = Extender[F, Int]
  type ExtenderString[F[_]] = Extender[F, String]
  type ExtenderIntStr[F[_]] = Extender[F, (Int, String)]

  class AskInt[M[_]: InvariantAskInt] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskString[M[_]: InvariantAskString] {
    def run: M[String] = InvariantAsk[M, String].ask
  }

  class AskIntString[M[_]: InvariantAskIntString] {
    def run: M[(Int, String)] = InvariantAsk[M, (Int, String)].ask
  }

  class AskIntBoolean[M[_]: InvariantAskIntBool] {
    def run: M[(Int, Boolean)] = InvariantAsk[M, (Int, Boolean)].ask
  }

  class AskIntBooleanString[M[_]: InvariantAskIntBoolStr] {
    def run: M[(Int, Boolean, String)] = InvariantAsk[M, (Int, Boolean, String)].ask
  }

  class StateString[M[_]: StatefulString] {
    def run: M[String] = Stateful[M, String].get
  }

  class StateIntString[M[_]: StatefulIntString] {
    def run: M[(Int, String)] = Stateful[M, (Int, String)].get
  }

  class StateAskIntBooleanString[M[_]: InvariantAskIntString: StatefulBoolean] {
    def run: M[(Int, Boolean, String)] = ???
  }

  class AddAskContext[M[_]](implicit C: Provider[M]) {

    val E: Extends[M,Unit,Int] = C.apply[Int]
    import E._

    val service = new AskInt[E.FE]

    def run: M[Int] = E.apply(3)(service.run)
  }

  class ComplexStateString[M[_]: StatefulString] {
    def run: M[String] = Stateful[M, String].get
  }

  class AddStateContext[M[_]: Provider] {

    val E: Extends[M,Unit,String] = implicitly[Provider[M]].apply[String]
    import E._

    val service = new ComplexStateString[E.FE]

    def run: M[String] = E.apply("Value")(service.run)
  }

  class Dependency[M[_], MC[_]](a: AskInt[MC])(implicit e: Provided[MC, M, Int]) {
    def run: M[Int] = e.apply(3)(a.run)
  }

  class Dependency2[M[_], MC[_], MC2[_]](a: AskIntString[MC2])(implicit
    e: Provided[MC, M, Int],
    e2: Provided[MC2, MC, String],
  ) {
    def run: M[(Int, String)] =
      e.apply(3)(e2.apply("value")(a.run))
  }

  class AskContext2[M[_]: Provider] {

    val e: Provides[M, String] =
      implicitly[Provider[M]].apply[String]
    import e._

    val nested = new AskContextNested[e.FE]()
  }

  class AskContextNested[M[_]: ExtenderString: InvariantAskString] {

    val string2 = new AskString[M]

    val e: Extends[M,String,Int] = implicitly[Extender[M, String]].apply[Int]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val string1 = new AskString[e.FE]

  }

  class AskContextNested2[M[_]: ExtenderIntStr] {
    import ContextProjector._

    val string2 = new AskString[M]

    val e: Extends[M,(Int, String),Boolean] = implicitly[Extender[M, (Int, String)]].apply[Boolean]
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

  class MixContextNested[M[_]: ExtenderString: StatefulString] {
    val string2 = new StateString[M]

    val string4: AskString[M] = {
      import ContextCombinator.{combinatorAsk, stateCombinator}
      new AskString[M]
    }

    val e: Extends[M,String,Int] = implicitly[Extender[M, String]].apply[Int]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new StateIntString[e.FE]

    val string1 = new StateString[e.FE]

    val string3 = new AskString[e.FE]
  }

  class AddContextMixed2[M[_]: ExtenderIntStr] {

    val (string2, intString, intString2) = {
      import ContextProjector._
      (new AskString[M], new AskIntString[M], new StateIntString[M])
    }

    val e: Extends[M,(Int, String),Boolean] = implicitly[Extender[M, (Int, String)]].apply[Boolean]
    import e._

    val service = new AskInt[e.FE]

    val service2 = new AskIntString[e.FE]

    val service3 = new StateIntString[e.FE]

    val service4 = new StateAskIntBooleanString[e.FE]

    val string1 = new AskString[e.FE]
  }

  class DoubleExtended[M[_]: ExtenderInt] {

    //Cannot have 2 Extends implicits in scope at the same time
    val ES: Extends[M, Int, String] = implicitly[Extender[M, Int]].apply[String]
    val service1: AskIntString[ES.FE] = {
      import ES._
      new AskIntString[ES.FE]
    }

    val EB: Extends[M, Int, Boolean] = implicitly[Extender[M, Int]].apply[Boolean]
    val service2: AskIntBoolean[EB.FE] = {
      import EB._
      new AskIntBoolean[EB.FE]
    }

    def run(): (M[(Int, String)], M[(Int, Boolean)]) =
      ES.apply("String")(service1.run) ->
        EB.apply(false)(service2.run)
  }

  class DoubleExtendedHierarchical[M[_]: ExtenderInt] {

    //Cannot have 2 Extends implicits in scope at the same time
    val ES: Extends[M,Int,Boolean] = implicitly[Extender[M, Int]].apply[Boolean]
    val service1: AskIntBoolean[ES.FE] = {
      import ES._
      new AskIntBoolean[ES.FE]
    }

    val EB: Extends[ES.FE,(Int, Boolean),String] = ES.extender.apply[String]
    val service2: AskIntBooleanString[EB.FE] = {
      import EB._
      new AskIntBooleanString[EB.FE]
    }

    def run(): (M[(Int, Boolean)], M[(Int, Boolean, String)]) =
      ES.apply(false)(service1.run) ->
        ES.apply(false)(EB.apply("bob")(service2.run))
  }
}
