package caio.mtl

import cats.Applicative
import cats.data.NonEmptyList

trait ApplicativeFail[F[_], V] {
  val applicative: Applicative[F]

  def fail[A](failure:V):F[A] =
    failMany(NonEmptyList(failure, Nil))

  def failMany[A](failures:NonEmptyList[V]):F[A]

  def handleFailuresWith[A](fa: F[A])(f: NonEmptyList[V] => F[A]): F[A]

  def inspect[A](fa: F[A]): F[Either[NonEmptyList[V], A]] =
    handleFailuresWith(
      applicative.map(fa)(Right(_): Either[NonEmptyList[V], A])
    )(e => applicative.pure(Left(e)))

  def resolve[A](fa: F[A])(pf: PartialFunction[NonEmptyList[V], A]): F[A] =
    handleFailuresWith(fa)(e => (pf.andThen(x => applicative.pure(x)).applyOrElse(e, failMany[A])))

  def resolveWith[A](fa: F[A])(pf: PartialFunction[NonEmptyList[V], F[A]]): F[A] =
    handleFailuresWith(fa)(e => pf.applyOrElse(e, failMany[A]))
}

object ApplicativeFail {
  def apply[F[_], V](implicit AF:ApplicativeFail[F, V]):ApplicativeFail[F, V] =
    AF
}

