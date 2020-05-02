package caio.std

import caio._
import cats.Monoid
import cats.effect.{IO, Sync}

class CaioSync[C, V, L:Monoid] extends CaioBracket[C, V, L] with Sync[Caio[C, V, L, *]] {
  def suspend[A](thunk: => Caio[C, V, L, A]): Caio[C, V, L, A] =
    CaioKleisli{ c =>
      IOResult(IO.suspend(thunk.toIOResult(c).io))
    }

}
