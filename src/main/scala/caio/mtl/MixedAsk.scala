package caio.mtl

import cats.Applicative
import cats.mtl.{Ask, Stateful}
import io.typechecked.alphabetsoup.Mixer

class MixedAsk[F[_], E1, E2](Ask: Ask[F, E1])(implicit mixer: Mixer[E1, E2]) extends InvariantAsk[F, E2] {

  val applicative: Applicative[F] =
    Ask.applicative

  def ask[E3 >: E2]: F[E3] =
    applicative.map(Ask.ask)(mixer.mix)

  override def reader[C](f: E2 => C): F[C] =
    Ask.reader[C](a => f(mixer.mix(a)))
}

class MixedState2Ask[F[_], E1, E2](state: Stateful[F, E1])(implicit mixer: Mixer[E1, E2]) extends InvariantAsk[F, E2] {

  val applicative: Applicative[F] =
    state.monad

  def ask[E3 >: E2]: F[E3] =
    applicative.map(state.get)(mixer.mix)

  override def reader[C](f: E2 => C): F[C] =
    state.inspect[C](a => f(mixer.mix(a)))
}
