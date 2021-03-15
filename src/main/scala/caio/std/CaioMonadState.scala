package caio.std

import caio.Caio
import cats.Monad
import cats.mtl.MonadState

class CaioMonadState[C, V, L] extends MonadState[Caio[C, V, L, *], C] {
  val monad: Monad[Caio[C, V, L, *]] =
    new CaioMonad[C, V, L]

  def get: Caio[C, V, L, C] =
    Caio.getContext

  def set(s: C): Caio[C, V, L, Unit] =
    Caio.setContext(s)

  def inspect[A](f: C => A): Caio[C, V, L, A] =
    Caio.getContext.map(f)

  def modify(f: C => C): Caio[C, V, L, Unit] =
    Caio.modifyContext(f)
}
