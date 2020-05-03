package caio.std

import caio.{Caio, HandleErrorCaio, IOCaio}
import cats.effect.IO
import cats.{MonadError, Monoid}

class CaioMonadError[C, V, L:Monoid]
  extends CaioMonad[C, V, L] with MonadError[Caio[C, V, L, *], Throwable] {

  def raiseError[A](e: Throwable): Caio[C, V, L, A] =
    IOCaio(IO.raiseError(e))

  def handleErrorWith[A](fa: Caio[C, V, L, A])(f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    HandleErrorCaio(fa, f)
}
