package caio.mtl

import cats.Applicative
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

class MixedApplicativeAsk[F[_], E1, E2](applicativeAsk:ApplicativeAsk[F, E1])(implicit mixer:Mixer[E1, E2])
  extends ApplicativeAsk[F, E2]{

  val applicative: Applicative[F] =
    applicativeAsk.applicative

  def ask: F[E2] =
    applicative.map(applicativeAsk.ask)(mixer.mix)

  def reader[C](f: E2 => C): F[C] =
    applicativeAsk.reader[C](a => f(mixer.mix(a)))
}

class MixedState2ApplicativeAsk[F[_], E1, E2](monadState:MonadState[F, E1])(implicit mixer:Mixer[E1, E2])
  extends ApplicativeAsk[F, E2]{

  val applicative: Applicative[F] =
    monadState.monad

  def ask: F[E2] =
    applicative.map(monadState.get)(mixer.mix)

  def reader[C](f: E2 => C): F[C] =
    monadState.inspect[C](a => f(mixer.mix(a)))
}

