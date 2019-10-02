package caio.std

import caio.mtl.ApplicativeFail
import caio._
import cats.data.NonEmptyList
import cats.{Applicative, Monoid}

class CaioApplicativeFail[C, V, L:Monoid] extends ApplicativeFail[Caio[C, V, L, *], V]{
  val applicative: Applicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def failMany[A](failures: NonEmptyList[V]): Caio[C, V, L, A] =
    CaioError(Right(failures), Store.empty)

  def handleFailuresWith[A](fa: Caio[C, V, L, A])(f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleFailures(f)
}
