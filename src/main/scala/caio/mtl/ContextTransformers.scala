package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError}
import cats.effect.{Async, Bracket, Concurrent, LiftIO, Sync}
import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextTransformers[F[_]]
  extends ContextWriterTransformers[F]
  with ContextFailTransformers[F] {
  type FE[A]

  def transformApplicative(A:Applicative[F]):Applicative[FE] =
    A.asInstanceOf[Applicative[FE]]

  def transformFunctor(F:Functor[F]):Functor[FE] =
    F.asInstanceOf[Functor[FE]]

  def transformMonad(M:Monad[F]):Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  def transformMonadError[E](M:MonadError[F, E]):MonadError[FE, E] =
    M.asInstanceOf[MonadError[FE, E]]

  def transformBracket[E](M:Bracket[F, E]):Bracket[FE, E] =
    M.asInstanceOf[Bracket[FE, E]]

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

trait ContextFailTransformers[F[_]] {
  type FE[A]

  def transformApplicativeFail[V](A:ApplicativeFail[F, V]):ApplicativeFail[FE, V] =
    A.asInstanceOf[ApplicativeFail[FE, V]]
}
