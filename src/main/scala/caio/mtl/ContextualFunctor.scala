package caio.mtl

import cats.effect.{Async, Bracket, Concurrent, Sync}
import cats.{Functor, Monad, MonadError}

trait ContextualFunctor {

  implicit def functorFE[F[_]](implicit F:ToFunctor[F]):Functor[F] =
    F.F
  
  implicit def functorToFunctor[F[_], FE[_]](implicit P:Transform[FE, F], F:Functor[F]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(F))

  implicit def monadToFunctor[F[_], FE[_]](implicit P:Transform[FE, F], M:Monad[F]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(M))

  implicit def monadErrorToFunctor[F[_], FE[_], E](implicit P:Transform[FE, F], M:MonadError[F, E]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(M))

  implicit def bracketToFunctor[F[_], FE[_], E](implicit P:Transform[FE, F], B:Bracket[F, E]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(B))

  implicit def syncToFunctor[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(S))

  implicit def asyncToFunctor[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(A))

  implicit def concurrentToFunctor[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToFunctor[FE] =
    ToFunctor(P.transformFunctor(C))
}

case class ToFunctor[F[_]](F:Functor[F]) extends AnyVal