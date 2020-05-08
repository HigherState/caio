package caio.std

import caio._
import cats.Monoid
import cats.effect.{IO, Sync}

class CaioSync[C, V, L:Monoid] extends CaioBracket[C, V, L] with Sync[Caio[C, V, L, *]] {
  def suspend[A](thunk: => Caio[C, V, L, A]): Caio[C, V, L, A] =
    KleisliCaio{ c =>
      FoldCaioIO(IO.suspend(Caio.foldIO(thunk, c)))
    }

  override def delay[A](thunk: => A): Caio[C, V, L, A] =
    IOCaio(IO.delay(thunk))
}
