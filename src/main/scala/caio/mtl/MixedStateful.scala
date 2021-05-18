package caio.mtl

import cats.Monad
import cats.mtl.Stateful
import io.typechecked.alphabetsoup.Mixer

class MixedStateful[F[_], E1, E2](state: Stateful[F, E1])(implicit mixer: Mixer[E1, E2]) extends Stateful[F, E2] {
  val monad: Monad[F] =
    state.monad

  def get: F[E2] =
    monad.map(state.get)(mixer.mix)

  def set(b: E2): F[Unit] =
    state.modify(a => mixer.inject(b, a))

  override def inspect[B](f: E2 => B): F[B] =
    state.inspect(a => f(mixer.mix(a)))

  override def modify(f: E2 => E2): F[Unit] =
    state.modify(a => mixer.modify(f)(a))
}
