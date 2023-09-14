package caio.std

import caio.Caio
import cats.Monad
import cats.mtl.Stateful

trait CaioStateful[C, L] extends Stateful[Caio[C, L, _], C] {
  val monad: Monad[Caio[C, L, _]] =
    CaioMonad[C, L]

  def get: Caio[C, L, C] =
    Caio.getContext[C]

  def set(s: C): Caio[C, L, Unit] =
    Caio.setContext(s)

  override def inspect[A](f: C => A): Caio[C, L, A] =
    Caio.getContext.map(f)

  override def modify(f: C => C): Caio[C, L, Unit] =
    Caio.modifyContext(f)
}

object CaioStateful {
  def apply[C, L]: CaioStateful[C, L] =
    new CaioStateful[C, L] {}
}
