package caio.mtl

import cats.effect.{Async, Concurrent, LiftIO}

trait ContextualLiftIO {

  implicit def syncFE[F[_]](implicit L:ToLiftIO[F]):LiftIO[F] =
    L.L

  implicit def liftIOToLiftIO[F[_], FE[_]](implicit P:Transform[FE, F], L:LiftIO[F]):ToLiftIO[FE] =
    ToLiftIO(P.transformLiftIO(L))

  implicit def asyncToLiftIO[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToLiftIO[FE] =
    ToLiftIO(P.transformLiftIO(A))

  implicit def concurrentToLiftIO[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToLiftIO[FE] =
    ToLiftIO(P.transformLiftIO(C))
}

case class ToLiftIO[F[_]](L:LiftIO[F]) extends AnyVal