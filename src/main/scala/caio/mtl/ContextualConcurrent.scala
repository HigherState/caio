package caio.mtl

import cats.effect.{Async, Concurrent}

trait ContextualConcurrent {

  implicit def concurrentFE[F[_]](implicit C:ToConcurrent[F]):Async[F] =
    C.C

  implicit def concurrentToConcurrent[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToAsync[FE] =
    ToAsync(P.transformAsync(C))
}

case class ToConcurrent[F[_]](C:Concurrent[F]) extends AnyVal