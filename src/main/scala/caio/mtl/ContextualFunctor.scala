package caio.mtl

import cats.effect.{Bracket, Sync}
import cats.{Functor, Monad, MonadError}

trait ContextualFunctor {

  implicit def functorFE[F[_]](implicit F:ToFunctor[F]):Functor[F] =
    F.apply()
}

sealed trait ToFunctor[F[_]] {
  def apply():Functor[F]
}
case class FunctorToFunctor[F[_], FE[_]](P:Transform[FE, F], F:Functor[F]) extends ToFunctor[FE] {
  def apply():Functor[FE] =
    P.transformFunctor(F)
}
case class MonadToFunctor[F[_], FE[_]](P:Transform[FE, F], M:Monad[F]) extends ToFunctor[FE]{
  def apply():Functor[FE] =
    P.transformFunctor(M)
}
case class MonadErrorToFunctor[F[_], FE[_], E](P:Transform[FE, F], M:MonadError[F, E]) extends ToFunctor[FE]{
  def apply():Functor[FE] =
    P.transformFunctor(M)
}
case class BracketToFunctor[F[_], FE[_], E](P:Transform[FE, F], B:Bracket[F, E]) extends ToFunctor[FE]{
  def apply():Functor[FE] =
    P.transformFunctor(B)
}
case class SyncToFunctor[F[_], FE[_]](P:Transform[FE, F], S:Sync[F]) extends ToFunctor[FE]{
  def apply():Functor[FE] =
    P.transformFunctor(S)
}
