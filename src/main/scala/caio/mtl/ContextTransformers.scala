package caio.mtl

import cats.{Applicative, Functor, Monad}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextTransformers[F[_]] {
  type FE[A]

  def transformApplicative(implicit A:Applicative[F]):Applicative[FE] =
    A.asInstanceOf[Applicative[FE]]

  def transformFunctor(implicit F:Functor[F]):Functor[FE] =
    F.asInstanceOf[Functor[FE]]

  def transformMonad(implicit M:Monad[F]):Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  def transformSync(implicit S:Sync[F]):Sync[FE] =
    S.asInstanceOf[Sync[FE]]

  def transformAsync(implicit A:Async[F]):Async[FE] =
    A.asInstanceOf[Async[FE]]

  def transformLiftIO(implicit L:LiftIO[F]):LiftIO[FE] =
    L.asInstanceOf[LiftIO[FE]]

  def transformConcurrent(implicit C:Concurrent[F]):Concurrent[FE] =
    C.asInstanceOf[Concurrent[FE]]

}


trait ContextWriterTransformers[F[_], L] {
  type FE[A]

  def transformFunctorTell(implicit F:FunctorTell[F, L]):FunctorTell[FE, L] =
    F.asInstanceOf[FunctorTell[FE, L]]

  def transformFunctorListen(implicit F:FunctorListen[F, L]):FunctorListen[FE, L] =
    F.asInstanceOf[FunctorListen[FE, L]]

  def transformApplicativeCensor(implicit A:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L] =
    A.asInstanceOf[ApplicativeCensor[FE, L]]
}
