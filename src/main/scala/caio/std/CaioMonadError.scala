package caio.std

import caio.{Caio, IOCaio}
import cats.MonadError
import cats.effect.kernel.{CancelScope, Poll}
import cats.effect.{IO, MonadCancel}

trait CaioMonadError[C, L] extends CaioMonad[C, L] with MonadError[Caio[C, L, *], Throwable] {
  def raiseError[A](ex: Throwable): Caio[C, L, A] =
    Caio.raiseError(ex)

  def handleErrorWith[A](fa: Caio[C, L, A])(f: Throwable => Caio[C, L, A]): Caio[C, L, A] =
    fa.handleErrorWith(f)
}

trait CaioMonadCancel[C, L] extends CaioMonadError[C, L] with MonadCancel[Caio[C, L, *], Throwable] {
  def rootCancelScope: CancelScope = CancelScope.Cancelable

  def forceR[A, B](fa: Caio[C, L, A])(fb: Caio[C, L, B]): Caio[C, L, B] = ???

  def uncancelable[A](body: Poll[Caio[C, L, *]] => Caio[C, L, A]): Caio[C, L, A] = ???

  def canceled: Caio[C, L, Unit] =
    IOCaio(IO.canceled)

  def onCancel[A](fa: Caio[C, L, A], fin: Caio[C, L, Unit]): Caio[C, L, A] = ???

}
