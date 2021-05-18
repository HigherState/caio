package caio.std

import caio.Caio
import caio.mtl.ApplicativeFail
import cats.CommutativeApplicative
import cats.data.NonEmptyList

class CaioApplicativeFail[C, V, L] extends ApplicativeFail[Caio[C, V, L, *], V] {
  val applicative: CommutativeApplicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def failMany[A](failures: NonEmptyList[V]): Caio[C, V, L, A] =
    Caio.failMany(failures)

  def handleFailuresWith[A](fa: Caio[C, V, L, A])(f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.handleFailuresWith(f)
}
