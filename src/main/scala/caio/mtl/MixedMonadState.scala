package caio.mtl

import cats.Monad
import cats.mtl.MonadState
import io.typechecked.alphabetsoup.Mixer

class MixedMonadState[F[_], E1, E2](monadState:MonadState[F, E1])(implicit mixer:Mixer[E1, E2]) extends MonadState[F, E2] {
  val monad: Monad[F] =
    monadState.monad

  def get: F[E2] =
    monad.map(monadState.get)(mixer.mix)

  def set(b: E2): F[Unit] =
    monadState.modify(a => mixer.inject(b, a))

  def inspect[B](f: E2 => B): F[B] =
    monadState.inspect (a => f(mixer.mix(a)))

  def modify(f: E2 => E2): F[Unit] =
    monadState.modify(a => mixer.modify(f)(a))
}
