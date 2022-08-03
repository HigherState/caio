package caio.std

import caio._
import cats.Monoid
import cats.effect.Sync

class CaioSync[C, L: Monoid] extends CaioBracket[C, L] with Sync[Caio[C, L, *]] {
  def suspend[A](thunk: => Caio[C, L, A]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      Caio.foldIO(thunk, c)
    }

  override def delay[A](thunk: => A): Caio[C, L, A] =
    Caio(thunk)
}
