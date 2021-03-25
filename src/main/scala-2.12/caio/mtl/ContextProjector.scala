package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait ContextProjector {

  implicit def projectMonadState[M[_], A]
    (implicit MSP: StateProjection[M, A]): MonadState[M, A] =
    MSP.MS

  implicit def projectApplicativeAsk[M[_], A]
    (implicit AP: AskProjection[M, A]): ApplicativeAsk[M, A] =
    AP.A

  implicit def extenderAskProjection[M[_], A, B]
    (implicit E: Extender[M, A], M: Mixer[A, B]): AskProjection[M, B] =
    AskProjection {
      new MixedApplicativeAsk[M, A, B](E.applicativeAsk)
    }

  implicit def askAskProjection[M[_], A, B]
    (implicit AA: ApplicativeAsk[M, A], M: Mixer[A, B], EV: A =:!= B): AskProjection[M, B] =
    AskProjection {
      new MixedApplicativeAsk[M, A, B](AA)
    }

  implicit def stateStateProjection[M[_], A, B]
    (implicit MS: MonadState[M, A], M: Mixer[A, B]): StateProjection[M, B] =
    StateProjection {
      new MixedMonadState[M, A, B](MS)
    }

  implicit def extenderStateProjection[M[_], A, B]
    (implicit E: Extender[M, A], M: Mixer[A, B]): StateProjection[M, B] =
    StateProjection {
      new MixedMonadState[M, A, B](E.monadState)
    }

}

case class AskProjection[M[_], A](A: ApplicativeAsk[M, A]) extends AnyVal
case class StateProjection[M[_], A](MS: MonadState[M, A]) extends AnyVal

object ContextProjector extends ContextProjector


