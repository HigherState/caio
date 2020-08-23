package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError}
import cats.effect.{Async, Bracket, Clock, Concurrent, LiftIO, Sync, Timer}
import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait TransformMtl[M2[_], M[_]] {

  def transformApplicative(A:Applicative[M]):Applicative[M2]

  def transformFunctor(F:Functor[M]):Functor[M2]

  def transformMonad(M:Monad[M]):Monad[M2]

  def transformMonadError[E](M:MonadError[M, E]):MonadError[M2, E]

  def transformBracket[E](M:Bracket[M, E]):Bracket[M2, E]

  def transformSync(S:Sync[M]):Sync[M2]

  def transformAsync(A:Async[M]):Async[M2]

  def transformLiftIO(L:LiftIO[M]):LiftIO[M2]

  def transformConcurrent(C:Concurrent[M]):Concurrent[M2]

  def transformApplicativeFail[V](A:ApplicativeFail[M, V]):ApplicativeFail[M2, V]

  def transformFunctorTell[L](F:FunctorTell[M, L]):FunctorTell[M2, L]

  def transformFunctorListen[L](F:FunctorListen[M, L]):FunctorListen[M2, L]

  def transformApplicativeCensor[L](F:ApplicativeCensor[M, L]):ApplicativeCensor[M2, L]

  def transformClock(C:Clock[M]):Clock[M2]

  def transformTimer(T:Timer[M]):Timer[M2]

  def transformEffectful(E:Effectful[M]):Effectful[M2]
}
