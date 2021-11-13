package caio.std

import caio._
import cats.effect.{IO, Sync}

trait CaioSync[C, L]
  extends CaioDefer[C, L]
    with CaioUnique[C, L]
    with CaioMonadCancel[C, L]
    with CaioClock[C, L]
    with Sync[Caio[C, L, *]] {

  def suspend[A](hint: Sync.Type)(thunk: => A): Caio[C, L, A] =
    IOCaio[A](IO.suspend(hint)(thunk))
}
