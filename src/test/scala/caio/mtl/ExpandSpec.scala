package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}

class ExpandSpec {

  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskString[M[_]:ApplicativeAsk[*[_], String]] {
    def run:M[String] = ApplicativeAsk[M, String].ask
  }

  class AskIntString[M[_]:ApplicativeAsk[*[_], (Int, String)]] {
    def run:M[(Int, String)] = ApplicativeAsk[M,(Int, String)].ask
  }

  class AskIntBoolean[M[_]:ApplicativeAsk[*[_], (Int, Boolean)]] {
    def run:M[(Int, Boolean)] = ApplicativeAsk[M,(Int, Boolean)].ask
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


  class AddAskContext[M[_]:Expander[?[_], Unit]] {
    import ContextProjector._

    val E = Expander.expand[M, Unit, Int]
    import E._

    val service = new AskInt[E.M2]

    def run:M[Int] = E.apply(3)(service.run)
  }

  class ComplexStateString[M[_]:MonadState[*[_], String]] {
    def run:M[String] = MonadState[M,String].get
  }

  class AddStateContext[M[_]:Expander[*[_], Unit]] {
    import ContextProjector._

    val E = Expander.expand[M, Unit, String]
    import E._

    val service = new ComplexStateString[E.M2]

    def run:M[String] = E.apply("Value")(service.run)
  }

  class AskContext2[M[_]:Expander[*[_], Unit]] {
    import ContextProjector._

    val e = Expander.expand[M, Unit, String]
    import e._

    val nested = new AskContextNested[e.M2]()
  }

  class AskContextNested[M[_]:Expander[*[_], String]] {
    import ContextProjector._

    val string2 = new AskString[M]

    val e = Expander.expand[M, String, Int]
    import e._

    val service = new AskInt[e.M2]

    val service2 = new AskIntString[e.M2]

    val string1 = new AskString[e.M2]

  }

  class AskContextNested2[M[_]:Expander[*[_], (Int, String)]] {
    import ContextProjector._

    val string2 = new AskString[M]

    val e = Expander.expand[M, (Int, String), Boolean]
    //TODO support with ContextProjector
    import e._

    val service = new AskInt[e.M2]

    val service2 = new AskIntString[e.M2]

    val service3 = new AskIntBooleanString[e.M2]

    val string1 = new AskString[e.M2]
  }


  class MixContext2[M[_]:Expander[*[_], Unit]] {
    import ContextProjector._

    val e = Expander.expand[M, Unit, String]
    import e._

    val state1 = new StateString[e.M2]

    val nested = new MixContextNested[e.M2]()
  }

  class MixContextNested[M[_]:Expander[*[_], String]] {
    import ContextProjector._

    val string2 = new StateString[M]

    val string4 = {
      import ContextCombinator._
      new AskString[M]
    }

    val e = Expander.expand[M, String, Int]
    import e._

    val service = new AskInt[e.M2]

    val service2 = new StateIntString[e.M2]

    val string1 = new StateString[e.M2]

    val string3 = new AskString[e.M2]
  }

  class AddContextMixed2[M[_]:Expander[*[_], (Int, String)]] {
    import ContextProjector._

    val (string2, intString, intString2) = {
      (new AskString[M], new AskIntString[M], new StateIntString[M])
    }

    val e = Expander.expand[M, (Int, String), Boolean]
    import e._

    val service = new AskInt[e.M2]

    val service2 = new AskIntString[e.M2]

    val service3 = new StateIntString[e.M2]

    val service4 = new StateAskIntBooleanString[e.M2]

    val string1 = new AskString[e.M2]
  }


  class DoubleExpanded[M[_]:Expander[*[_], Int]] {
    import ContextProjector._
    //Cannot have 2 Extends implicits in scope at the same time
    val ES = Expander.expand[M, Int, String]
    val service1:AskIntString[ES.M2] = {
      import ES._
      new AskIntString[ES.M2]
    }

    val EB = Expander.expand[M, Int, Boolean]
    val service2 = {
      import EB._
      new AskIntBoolean[EB.M2]
    }

    def run():(M[(Int, String)], M[(Int, Boolean)]) =
      ES.apply("String")(service1.run) ->
      EB.apply(false)(service2.run)


  }

  class DoubleExtendedHierarchical[M[_]:Expander[*[_], Int]] {
    import ContextProjector._

    //Cannot have 2 Extends implicits in scope at the same time
    val ES = Expander.expand[M, Int, Boolean]

    val service1:AskIntBoolean[ES.M2] = {
      import ES._
      new AskIntBoolean[ES.M2]
    }

    val EB: Expanded[ES.M2, (Int, Boolean), String] = ES.expands.expand[String]
    val service2 = {
      import EB._
//TODO: Why wont this resolve
      val c = ContextProjector.expanderAskProjection[EB.M2, ((Int, Boolean), String), (Int, Boolean, String)]
      new AskIntBooleanString[EB.M2]()(c.A)
    }

    def run() =
      ES.apply(false)(service1.run) ->
        ES.apply(false)(EB.apply("bob")(service2.run))


  }



  class ExpandsDependency[M[_], MC[_]:Expands[*[_], M, Int]]
  (a:AskInt[MC]){


    def run:M[Int] = Expands(3, a.run)
  }

  class ExpandsDependency2[M[_],
    MC[_]:Expands[*[_], M, Int],
    MC2[_]:Expands[*[_], MC, String]
  ](a:AskIntString[MC2]){


    def run:M[(Int, String)] =
      Expands(3, Expands("value", a.run))
  }

  //  class ExtendDependency[M[_], MC[_]:Expands[*[_], M, Int]] {
  //    import ContextProjector._
  //
  //    val e = Expander.expand[M, Unit, String]
  //    import e._
  //
  //    def run:M[Int] = Expands(3, a.run)
  //  }

  class ExpandsAsk2Ask[
    M[_]:ApplicativeAsk[*[_], String],
    M2[_]:Expands[*[_], M, Int]
  ]{
    import ContextProjector._
    val AP = ContextProjector.expandsApplicativeAskAskProjection[M, M2, String, Int, (Int, String)]
    val a = new AskIntString[M2]()(AP.A)
  }

  class ExpandsExpander2Ask[
    M[_]:Expander[*[_], String],
    M2[_]:Expands[*[_], M, Int]
  ]{
    import ContextProjector._

    val a = new AskIntString[M2]
  }

  class ExpandsState2State[
    M[_]:MonadState[*[_], String],
    M2[_]:Expands[*[_], M, Int]
  ]{
    import ContextProjector._

    val a = new StateIntString[M2]
  }

  class ExpandsExpander2State[
    M[_]:Expander[*[_], String],
    M2[_]:Expands[*[_], M, Int]
  ]{
    import ContextProjector._

    val a = new StateIntString[M2]
  }

  class ExpandsState2Ask[
    M[_]:MonadState[*[_], Int],
    M2[_]:Expands[*[_], M, String]
  ]{
    import ContextProjector._

    val a = new AskIntString[M2]
  }

}
