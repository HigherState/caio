package caio.mtl

import cats.{Applicative, ApplicativeError, Monad, MonadError}
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

  implicit def fromApplicativeError[F[_], V](implicit AE: ApplicativeError[F, NonEmptyList[V]]): ApplicativeFail[F, V] =
    new ApplicativeFail[F, V] {
      val applicative: Applicative[F] = AE

      def failMany[A](failures: NonEmptyList[V]): F[A] =
        AE.raiseError(failures)

      def handleFailuresWith[A](fa: F[A])(f: NonEmptyList[V] => F[A]): F[A] =
        AE.handleErrorWith(fa)(f)
    }

  implicit def applicativeError[F[_], V](implicit AF:ApplicativeFail[F, V]):ApplicativeError[F, NonEmptyList[V]] =
    new ApplicativeError[F, NonEmptyList[V]]{
      def raiseError[A](e: NonEmptyList[V]): F[A] =
        AF.failMany(e)

      def handleErrorWith[A](fa: F[A])(f: NonEmptyList[V] => F[A]): F[A] =
        AF.resolveWith(fa){case nel => f(nel)}

      def pure[A](x: A): F[A] =
        AF.applicative.pure(x)

      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
        AF.applicative.ap(ff)(fa)
    }

  implicit def monadError[F[_], V](implicit AF:ApplicativeFail[F, V], M:Monad[F]):MonadError[F, NonEmptyList[V]] =
    new MonadError[F, NonEmptyList[V]] {

      def raiseError[A](e: NonEmptyList[V]): F[A] =
        AF.failMany(e)

      def handleErrorWith[A](fa: F[A])(f: NonEmptyList[V] => F[A]): F[A] =
        AF.resolveWith(fa){case nel => f(nel)}

      def pure[A](x: A): F[A] =
        AF.applicative.pure(x)

      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
        M.flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
        M.tailRecM(a)(f)
    }
}

