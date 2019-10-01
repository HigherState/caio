package caio.std

import caio.{Caio, CaioError, Store}
import cats.{ApplicativeError, Monoid}

class CaioApplicativeError[C, V, L:Monoid] extends CaioApplicative[C, V, L] with ApplicativeError[Caio[C, V, L, *], Throwable] {
  def raiseError[A](e: Throwable): Caio[C, V, L, A] =
    CaioError(e, Store.empty[C, L])

  def handleErrorWith[A](fa: Caio[C, V, L, A])(f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleError(f)

}
