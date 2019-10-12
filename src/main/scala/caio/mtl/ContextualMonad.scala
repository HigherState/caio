package caio.mtl

import cats.effect.{Async, Bracket, Concurrent, Sync}
import cats.{Monad, MonadError}

trait ContextualMonad {

  implicit def monadFE[F[_]](implicit M:ToMonad[F]):Monad[F] =
    M.M

  implicit def monadToMonad[F[_], FE[_]](implicit P:Transform[FE, F], M:Monad[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(M))

  implicit def monadErrorToMonad[F[_], FE[_], E](implicit P:Transform[FE, F], M:MonadError[F, E]):ToMonad[FE] =
    ToMonad(P.transformMonad(M))

  implicit def bracketToMonad[F[_], FE[_], E](implicit P:Transform[FE, F], M:Bracket[F, E]):ToMonad[FE] =
    ToMonad(P.transformMonad(M))

  implicit def syncToMonad[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(S))

  implicit def asyncToMonad[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(A))

  implicit def concurrentToMonad[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToMonad[FE] =
    ToMonad(P.transformMonad(C))
}

case class ToMonad[F[_]](M:Monad[F]) extends AnyVal