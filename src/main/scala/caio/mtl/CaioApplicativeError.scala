package caio.mtl

import caio._
import cats.ApplicativeError

trait CaioApplicativeError extends ApplicativeError[Caio, Throwable] with CaioApplicative {
  def raiseError[A](e: Throwable): Caio[A] =
    CaioError(e, Store.empty)

  def handleErrorWith[A](fa: Caio[A])(f: Throwable => Caio[A]): Caio[A] =
    fa.handleError(f)

}
