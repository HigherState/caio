package caio.std

import caio.Caio
import cats.ApplicativeError

trait CaioApplicativeError[C, L]
    extends CaioApplicative[C, L]
    with ApplicativeError[Caio[C, L, *], Throwable] {
  def raiseError[A](ex: Throwable): Caio[C, L, A] =
    Caio.raiseError(ex)

  def handleErrorWith[A](fa: Caio[C, L, A])(f: Throwable => Caio[C, L, A]): Caio[C, L, A] =
    fa.handleErrorWith(f)
}
