package caio.mtl

import cats.effect.{Async, Concurrent}

trait ContextualAsync {

  implicit def asyncFE[F[_]](implicit A:ToAsync[F]):Async[F] =
    A.A

  implicit def asyncToAsync[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToAsync[FE] =
    ToAsync(P.transformAsync(A))

  implicit def concurrentToAsync[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToAsync[FE] =
    ToAsync(P.transformAsync(C))
}

case class ToAsync[F[_]](A:Async[F]) extends AnyVal