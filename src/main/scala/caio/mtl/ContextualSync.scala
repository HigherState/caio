package caio.mtl

import cats.effect.{Async, Concurrent, Sync}

trait ContextualSync {

  implicit def syncFE[F[_]](implicit S:ToSync[F]):Sync[F] =
    S.S

  implicit def syncToSync[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToSync[FE] =
    ToSync(P.transformSync(S))

  implicit def asyncToSync[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToSync[FE] =
    ToSync(P.transformSync(A))

  implicit def concurrentToSync[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToSync[FE] =
    ToSync(P.transformSync(C))
}

case class ToSync[F[_]](S:Sync[F]) extends AnyVal