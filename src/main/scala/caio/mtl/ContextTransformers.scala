package caio.mtl

import cats.{Applicative, Functor, Monad}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextTransformers[F[_]] {
  type FE[A]

  def transformApplicative(A:Applicative[F]):Applicative[FE] =
    A.asInstanceOf[Applicative[FE]]

  def transformFunctor(F:Functor[F]):Functor[FE] =
    F.asInstanceOf[Functor[FE]]

  def transformMonad(M:Monad[F]):Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  def transformMonadError(M:Monad[F]):Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  def transformSync(S:Sync[F]):Sync[FE] =
    S.asInstanceOf[Sync[FE]]

  def transformAsync(A:Async[F]):Async[FE] =
    A.asInstanceOf[Async[FE]]

  def transformLiftIO(L:LiftIO[F]):LiftIO[FE] =
    L.asInstanceOf[LiftIO[FE]]

  def transformConcurrent(C:Concurrent[F]):Concurrent[FE] =
    C.asInstanceOf[Concurrent[FE]]

}


trait ContextWriterTransformers[F[_]] {
  type FE[A]

  def transformFunctorTell[L](F:FunctorTell[F, L]):FunctorTell[FE, L] =
    F.asInstanceOf[FunctorTell[FE, L]]

  def transformFunctorListen[L](F:FunctorListen[F, L]):FunctorListen[FE, L] =
    F.asInstanceOf[FunctorListen[FE, L]]

  def transformApplicativeCensor[L](A:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L] =
    A.asInstanceOf[ApplicativeCensor[FE, L]]
}
