package caio.std

import caio._
import cats.{Eq, Monoid}
import cats.effect.{IO, Sync}

class CaioSync[C, V, L:Monoid:Eq] extends CaioBracket[C, V, L] with Sync[Caio[C, V, L, *]] {
  def suspend[A](thunk: => Caio[C, V, L, A]): Caio[C, V, L, A] =
    KleisliCaio{ c =>
      FoldCaioIO(IO.suspend(Caio.foldIO(thunk, c)))
    }
}
