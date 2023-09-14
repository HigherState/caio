package caio.mtl

import cats.mtl.Stateful
import io.typechecked.alphabetsoup.Mixer

trait ContextProjector extends ContextProjectorLowPriority {

  implicit def projectStateful[M[_], A](implicit MSP: StateProjection[M, A]): Stateful[M, A] =
    MSP.MS

  implicit def projectAsk[M[_], A](implicit AP: AskProjection[M, A]): InvariantAsk[M, A] =
    AP.A

  implicit def extenderAskProjection[M[_], A, B](implicit E: Extender[M, A], M: Mixer[A, B]): AskProjection[M, B] =
    AskProjection {
      new MixedAsk[M, A, B](E.applicativeAsk)
    }

  implicit def extenderStateProjection[M[_], A, B](implicit E: Extender[M, A], M: Mixer[A, B]): StateProjection[M, B] =
    StateProjection {
      new MixedStateful[M, A, B](E.state)
    }
}

trait ContextProjectorLowPriority {
  implicit def askAskProjection[M[_], A, B](implicit
    AA: InvariantAsk[M, A],
    M: Mixer[A, B]
  ): AskProjection[M, B] =
    AskProjection {
      new MixedAsk[M, A, B](AA)
    }

  implicit def stateStateProjection[M[_], A, B](implicit
    MS: Stateful[M, A],
    M: Mixer[A, B]
  ): StateProjection[M, B] =
    StateProjection {
      new MixedStateful[M, A, B](MS)
    }
}
case class AskProjection[M[_], A](A: InvariantAsk[M, A]) extends AnyVal
case class StateProjection[M[_], A](MS: Stateful[M, A])  extends AnyVal

object ContextProjector extends ContextProjector
