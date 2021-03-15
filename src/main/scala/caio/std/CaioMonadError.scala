package caio.std

import caio.Caio
import cats.MonadError

class CaioMonadError[C, V, L] extends CaioMonad[C, V, L] with MonadError[Caio[C, V, L, *], Throwable] {
  def raiseError[A](ex: Throwable): Caio[C, V, L, A] =
    Caio.raiseError(ex)

  def handleErrorWith[A](fa: Caio[C, V, L, A])(f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleErrorWith(f)
}
