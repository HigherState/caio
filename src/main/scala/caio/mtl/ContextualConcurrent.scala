package caio.mtl

import cats.Monad
import cats.effect.{Concurrent, LiftIO, Sync}

//Only three levels can be mapped without implicit divergence
trait ContextualConcurrent {

  implicit def monadFE[F[_]](implicit M:ToMonad[F]):Monad[F] =
    M.M

  implicit def syncFE[F[_]](implicit S:ToSync[F]):Sync[F] =
    S.S

  implicit def liftIOFE[F[_]](implicit L:ToLiftIO[F]):LiftIO[F] =
    L.L


  implicit def concurrentFE[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):Concurrent[FE] =
    P.transformConcurrent(C)

  implicit def monadToMonad[F[_], FE[_]](implicit P:Transform[FE, F], M:Monad[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(M))

  implicit def syncToMonad[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(S))

  implicit def concurrentToMonad[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(C))

  implicit def syncToSync[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToSync[FE] =
    ToSync(P.transformSync(S))

  implicit def concurrentToSync[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToSync[FE] =
    ToSync(P.transformSync(C))

  implicit def liftIOToLiftIO[F[_], FE[_]](implicit P:Transform[FE, F], L:LiftIO[F]):ToLiftIO[FE] =
    ToLiftIO(P.transformLiftIO(L))

  implicit def ConcurrentToLiftIO[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToLiftIO[FE] =
    ToLiftIO(P.transformLiftIO(C))
}
//inheritance here?
case class ToMonad[F[_]](M:Monad[F]) extends AnyVal
case class ToSync[F[_]](S:Sync[F]) extends AnyVal
case class ToConcurrent[F[_]](C:Concurrent[F]) extends AnyVal

case class ToLiftIO[F[_]](L:LiftIO[F]) extends AnyVal
