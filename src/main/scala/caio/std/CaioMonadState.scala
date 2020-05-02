package caio.std

import caio._
import cats.{Monad, Monoid}
import cats.mtl.MonadState

class CaioMonadState[C, V, L](implicit M:Monoid[L]) extends MonadState[Caio[C, V, L, *], C] {
  val monad: Monad[Caio[C, V, L, *]] =
    new CaioMonad[C, V, L] {}

  def get: Caio[C, V, L, C] =
    CaioKleisli(c => SuccessResult(c, EmptyStore))

  def set(s: C): Caio[C, V, L, Unit] =
    CaioState((), ContentStore(s, M.empty, M))

  def inspect[A](f: C => A): Caio[C, V, L, A] =
    get.map(f)

  def modify(f: C => C): Caio[C, V, L, Unit] =
    get.flatMap(s => set(f(s)))
}
