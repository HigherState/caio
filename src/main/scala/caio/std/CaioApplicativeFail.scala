package caio.std

import caio.mtl.ApplicativeFail
import caio._
import cats.{Applicative, Monoid}

class CaioApplicativeFail[C, V, L:Monoid] extends ApplicativeFail[Caio[C, V, L, *], V]{
  val applicative: Applicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def fail[A](failure: V): Caio[C, V, L, A] =
    CaioFail(failure, Store.empty)

  def handleFailureWith[A](fa: Caio[C, V, L, A])(f: V => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleFailure(f)
}
