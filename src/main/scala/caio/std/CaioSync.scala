package caio.std

import caio.Caio
import cats.effect.kernel.CancelScope
import cats.effect.{IO, Sync}

trait CaioSync[C, L] extends CaioMonadCancel[C, L] with CaioClock[C, L] with Sync[Caio[C, L, *]] {
  def suspend[A](hint: Sync.Type)(thunk: => A): Caio[C, L, A] =
    Caio.liftIO(IO.suspend(hint)(thunk))

  override def blocking[A](thunk: => A): Caio[C, L, A] =
    Caio.blocking[A](thunk)
}

object CaioSync {
  def apply[C, L]: CaioSync[C, L] =
    new CaioSync[C, L] {
      final override def rootCancelScope: CancelScope =
        CancelScope.Cancelable
    }
}
