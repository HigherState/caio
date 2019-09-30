package caio.mtl

trait ApplicativeFail[F[_], V] {

  def fail[A](failure:V):F[V]

  def handleFailureWith[A](fa: F[A])(f: V => F[A]): F[A]
}
