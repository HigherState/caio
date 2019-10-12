package caio.mtl

import cats.effect.{Async, Bracket, Concurrent, Sync}

trait ContextualBracket {

  implicit def bracketFE[F[_], E](implicit B:ToBracket[F, E]):Bracket[F, E] =
    B.B

  implicit def bracketToBracket[F[_], FE[_], E](implicit P:Transform[FE, F], M:Bracket[F, E]):ToBracket[FE, E] =
    ToBracket(P.transformBracket(M))

  implicit def syncToBracket[F[_], FE[_]](implicit P:Transform[FE, F], S:Sync[F]):ToBracket[FE, Throwable] =
    ToBracket(P.transformBracket(S))

  implicit def asyncToBracket[F[_], FE[_]](implicit P:Transform[FE, F], A:Async[F]):ToBracket[FE, Throwable] =
    ToBracket(P.transformBracket(A))

  implicit def concurrentToBracket[F[_], FE[_]](implicit P:Transform[FE, F], C:Concurrent[F]):ToBracket[FE, Throwable] =
    ToBracket(P.transformBracket(C))
}

case class ToBracket[F[_], E](B:Bracket[F, E]) extends AnyVal