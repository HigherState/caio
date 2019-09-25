package caio.mtl

import caio._
import cats.effect.{IO, Sync}

trait CaioSync extends Sync[Caio] with CaioBracket {
  def suspend[A](thunk: => Caio[A]): Caio[A] =
    CaioKleisli{ c =>
      IOResult(IO.suspend(thunk.toIOResult(c).io))
    }

}
