package caio.mtl

import cats.effect.{Async, Bracket, Concurrent, Sync}
import cats.{Applicative, Monad, MonadError}

trait ContextualApplicative {

  implicit def applicativeFE[F[_]](implicit F:ToApplicative[F]):Applicative[F] =
    F.A
  
  implicit def applicativeToApplicative[F[_], FE[_]](implicit P:Transform[FE, F], F:Applicative[F]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(F))

  implicit def monadToApplicative[F[_], FE[_]](implicit P:Transform[FE, F], M:Monad[F]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(M))

  implicit def monadErrorToApplicative[F[_], FE[_], E](implicit P:Transform[FE, F], M:MonadError[F, E]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(M))

  implicit def bracketToApplicative[F[_], FE[_], E](implicit P:Transform[FE, F], B:Bracket[F, E]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(B))

  implicit def syncToApplicative[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(S))

  implicit def asyncToApplicative[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(A))

  implicit def concurrentToApplicative[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToApplicative[FE] =
    ToApplicative(P.transformApplicative(C))
}

case class ToApplicative[F[_]](A:Applicative[F]) extends AnyVal

