package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.{=:!=, LowPriority}

trait ContextProjector {

  implicit def projectMonadState[M[_], A]
    (implicit MSP: StateProjection[M, A]): MonadState[M, A] =
    MSP.MS

  implicit def projectApplicativeAsk[M[_], A]
    (implicit AP: AskProjection[M, A]): ApplicativeAsk[M, A] =
    AP.A

  implicit def projectExpander[M[_], A]
  (implicit E:ExpanderProjection[M, A]): Expander[M, A] =
    E.E

  implicit def extenderAskProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):AskProjection[M, B] =
    AskProjection{
      new MixedApplicativeAsk[M, A, B](E.applicativeAsk)
    }

  implicit def expanderAskProjection[M[_], A, B]
    (implicit E:Expander[M, A], M:Mixer[A, B]):AskProjection[M, B] =
      ???

  implicit def askAskProjection[M[_], A, B]
    (implicit AA:ApplicativeAsk[M, A], M:Mixer[A, B], EV: A =:!= B):AskProjection[M, B] =
    AskProjection{
      new MixedApplicativeAsk[M, A, B](AA)
    }

  implicit def stateStateProjection[M[_], A, B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    StateProjection{
      new MixedMonadState[M, A, B](MS)
    }

  implicit def stateAskProjection[M[_], A, B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B], ev: LowPriority):AskProjection[M, B] =
    AskProjection[M, B]{
      new MixedState2ApplicativeAsk(MS)
    }

  implicit def extenderStateProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    StateProjection{
      new MixedMonadState[M, A, B](E.monadState)
    }

  implicit def expanderStateProjection[M[_], A, B]
    (implicit E:Expander[M, A], M:Mixer[A, B]):StateProjection[M, B] =
      ???

  implicit def expanderExpanderProjection[M[_], A, B]
    (implicit E:Expander[M, A], M:Mixer[A, B]):ExpanderProjection[M, B] =
      ???

  implicit def expandsApplicativeAskAskProjection[M[_], M2[_], A, B, C]
    (implicit E:Expands[M2, M, B], A:ApplicativeAsk[M, A], M:Mixer[(A, B), C]):AskProjection[M2, C] = ???

  implicit def expandsExpanderAskProjection[M[_], M2[_], A, B, C]
  (implicit E:Expands[M2, M, B], E2:Expander[M, A], M:Mixer[(A, B), C]):AskProjection[M2, C] = ???

//  implicit def expandsApplicativeMonadStateAskProjection[M[_], M2[_], A, B, C]
//  (implicit E:Expands[M2, M, B], A:MonadState[M, A], M:Mixer[(A, B), C]):AskProjection[M2, C] = ???
//
  implicit def expandsExpanderStateProjection[M[_], M2[_], A, B, C]
  (implicit E:Expands[M2, M, B], E2:Expander[M, A], M:Mixer[(A, B), C]):StateProjection[M2, C] = ???

  implicit def expandsApplicativeMonadStateStateProjection[M[_], M2[_], A, B, C]
  (implicit E:Expands[M2, M, B], A:MonadState[M, A], M:Mixer[(A, B), C]):StateProjection[M2, C] = ???
}



case class AskProjection[M[_], A](A:ApplicativeAsk[M, A]) extends AnyVal
case class ExpanderProjection[M[_], A](E:Expander[M, A]) extends AnyVal
case class StateProjection[M[_], A](MS:MonadState[M, A]) extends AnyVal

object ContextProjector extends ContextProjector


