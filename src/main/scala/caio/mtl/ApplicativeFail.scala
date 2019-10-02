package caio.mtl

import cats.Applicative
import cats.data.NonEmptyList

trait ApplicativeFail[F[_], V] {
  val applicative: Applicative[F]

  def fail[A](failure:V):F[A] =
    failMany(NonEmptyList(failure, Nil))

  def failMany[A](failures:NonEmptyList[V]):F[A]

  def handleFailuresWith[A](fa: F[A])(f: NonEmptyList[V] => F[A]): F[A]
}
