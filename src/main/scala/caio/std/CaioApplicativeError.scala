package caio.std

import caio.{Caio, CaioError, EmptyStore}
import cats.{ApplicativeError, Monoid}

class CaioApplicativeError[C, V, L:Monoid] extends CaioApplicative[C, V, L] with ApplicativeError[Caio[C, V, L, *], Throwable] {
  def raiseError[A](e: Throwable): Caio[C, V, L, A] =
    CaioError(Left(e), EmptyStore)

  def handleErrorWith[A](fa: Caio[C, V, L, A])(f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleError(f)

}
