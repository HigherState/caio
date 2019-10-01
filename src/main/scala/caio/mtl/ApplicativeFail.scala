package caio.mtl

import cats.Applicative

trait ApplicativeFail[F[_], V] {
  val applicative: Applicative[F]

  def fail[A](failure:V):F[A]

  def handleFailureWith[A](fa: F[A])(f: V => F[A]): F[A]
}
