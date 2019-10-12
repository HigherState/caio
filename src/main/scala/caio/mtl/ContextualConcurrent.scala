package caio.mtl

import cats.effect.Concurrent

trait ContextualConcurrent {

  implicit def concurrentFE[F[_]](implicit C:ToConcurrent[F]):Concurrent[F] =
    C.C

  implicit def concurrentToConcurrent[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToConcurrent[FE] =
    ToConcurrent(P.transformConcurrent(C))
}

case class ToConcurrent[F[_]](C:Concurrent[F]) extends AnyVal