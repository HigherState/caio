package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError}
import cats.effect.{Async, Bracket, Clock, Concurrent, LiftIO, Sync, Timer}
import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait Transformer {
  implicit def transformApplicative[M2[_], M[_], A](implicit E:Expands[M2, M, A], A:Applicative[M]):Applicative[M2] =
    E.transformApplicative(A)

  implicit def transformFunctor[M2[_], M[_], A](implicit E:Expands[M2, M, A], F:Functor[M]):Functor[M2] =
    E.transformFunctor(F)

  implicit def transformMonad[M2[_], M[_], A](implicit E:Expands[M2, M, A], M:Monad[M]):Monad[M2] =
    E.transformMonad(M)

  implicit def transformMonadError[M2[_], M[_], A, E](implicit E:Expands[M2, M, A], M:MonadError[M, E]):MonadError[M2, E] =
    E.transformMonadError(M)

  implicit def transformBracket[M2[_], M[_], A, E](implicit E:Expands[M2, M, A], M:Bracket[M, E]):Bracket[M2, E] =
    E.transformBracket(M)

  implicit def transformSync[M2[_], M[_], A](implicit E:Expands[M2, M, A], S:Sync[M]):Sync[M2] =
    E.transformSync(S)

  implicit def transformAsync[M2[_], M[_], A](implicit E:Expands[M2, M, A], A:Async[M]):Async[M2] =
    E.transformAsync(A)

  implicit def transformLiftIO[M2[_], M[_], A](implicit E:Expands[M2, M, A], L:LiftIO[M]):LiftIO[M2] =
    E.transformLiftIO(L)

  implicit def transformConcurrent[M2[_], M[_], A](implicit E:Expands[M2, M, A], C:Concurrent[M]):Concurrent[M2] =
    E.transformConcurrent(C)

  implicit def transformApplicativeFail[M2[_], M[_], A, V](implicit E:Expands[M2, M, A], A:ApplicativeFail[M, V]):ApplicativeFail[M2, V] =
    E.transformApplicativeFail(A)

  implicit def transformFunctorTell[M2[_], M[_], A, L](implicit E:Expands[M2, M, A], F:FunctorTell[M, L]):FunctorTell[M2, L] =
    E.transformFunctorTell(F)

  implicit def transformFunctorListen[M2[_], M[_], A, L](implicit E:Expands[M2, M, A], F:FunctorListen[M, L]):FunctorListen[M2, L] =
    E.transformFunctorListen(F)

  implicit def transformApplicativeCensor[M2[_], M[_], A, L](implicit E:Expands[M2, M, A], F:ApplicativeCensor[M, L]):ApplicativeCensor[M2, L] =
    E.transformApplicativeCensor(F)

  implicit def transformClock[M2[_], M[_], A](implicit E:Expands[M2, M, A], C:Clock[M]):Clock[M2] =
    E.transformClock(C)

  implicit def transformTimer[M2[_], M[_], A](implicit E:Expands[M2, M, A], T:Timer[M]):Timer[M2] =
    E.transformTimer(T)

  implicit def transformEffectful[M2[_], M[_], A](implicit E:Expands[M2, M, A], EF:Effectful[M]):Effectful[M2] =
    E.transformEffectful(EF)
}
