package caio.std

import caio.Caio
import cats.MonadError

trait CaioMonadError[C, L] extends CaioMonad[C, L] with MonadError[Caio[C, L, *], Throwable] {
  def raiseError[A](ex: Throwable): Caio[C, L, A] =
    Caio.raiseError(ex)

  def handleErrorWith[A](fa: Caio[C, L, A])(f: Throwable => Caio[C, L, A]): Caio[C, L, A] =
    fa.handleErrorWith(f)
}

object CaioMonadError {
  def apply[C, L]: CaioMonadError[C, L] =
    new CaioMonadError[C, L] {}
}
