package caio.mtl

import cats.effect.{Async, Bracket, Concurrent, Sync}
import cats.MonadError

trait ContextualMonadError {

  implicit def monadErrorFE[F[_], E](implicit M:ToMonadError[F, E]):MonadError[F, E] =
    M.M

  implicit def monadErrorToMonadError[F[_], FE[_], E](implicit P:Transform[FE, F], M:MonadError[F, E]):ToMonadError[FE, E] =
    ToMonadError(P.transformMonadError(M))

  implicit def bracketToMonadError[F[_], FE[_], E](implicit P:Transform[FE, F], M:Bracket[F, E]):ToMonadError[FE, E] =
    ToMonadError(P.transformMonadError(M))

  implicit def syncToMonadError[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToMonadError[FE, Throwable] =
    ToMonadError(P.transformMonadError(S))

  implicit def asyncToMonadError[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToMonadError[FE, Throwable] =
    ToMonadError(P.transformMonadError(A))

  implicit def concurrentToMonadError[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToMonadError[FE, Throwable] =
    ToMonadError(P.transformMonadError(C))
}

case class ToMonadError[F[_], E](M:MonadError[F, E]) extends AnyVal
