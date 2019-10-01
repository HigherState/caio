package caio.mtl

import cats.{Monad, MonadError}
import cats.effect._
import cats.mtl._

trait Context[F[_], L, E, V] {

  def apply[C]:EnvironmentContext[F, L, E, V, C]
}


trait EnvironmentContext[F[_], L, E, V, C] {

  type FC[A]

  def applicativeAsk:ApplicativeAsk[FC, C]

  def monadState:MonadState[FC, C]

  def monadError(implicit ME:MonadError[F, E]):MonadError[FC, E] = ???

  def sync(implicit S:Sync[F]):Sync[FC] = ???

  def monad(implicit M:Monad[F]):Monad[FC] = ???

  def async(implicit A:Async[F]):Async[FC] = ???

  def bracket(implicit A:Bracket[F, E]):Bracket[FC, E] = ???

  def liftIO(implicit L:LiftIO[F]):LiftIO[FC] = ???

  def concurrent(implicit C:Concurrent[F]):Concurrent[FC] = ???

  def concurrentEffect(implicit C:ConcurrentEffect[F]):ConcurrentEffect[FC] = ???

  def applicativeFail(implicit A:ApplicativeFail[F, V]):ApplicativeFail[FC, V] = ???

  def apply[A](c:C)(f:FC[A]):F[A]
}

case class EnvironmentWrapper[FC[_], F[_], L, E, V, C, A](fa:FC[A], A:WithContext[FC, F, L, E, V, C]) {
  def applyContext(c:C):F[A] =
    A.apply(c)(fa)
}

trait EnvironmentLift {

  implicit def toAux[F[_], L, E, V, C](implicit E:EnvironmentContext[F, L, E, V, C]): WithContext[E.FC, F, L, E, V, C] =
    E.asInstanceOf[WithContext[E.FC, F, L, E, V, C]]

  implicit def applicativeAskFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C]):ApplicativeAsk[FC, C] =
    A.applicativeAsk

  implicit def monadStateFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C]):MonadState[FC, C] =
    A.monadState

  implicit def monadErrorFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], ME:MonadError[F, E]):MonadError[FC, E] =
   A.monadError

  implicit def liftIOFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], L:LiftIO[F]):LiftIO[FC] =
    A.liftIO

  implicit def syncFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], S:Sync[F]):Sync[FC] =
    A.sync

  implicit def monadFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], M:Monad[F]):Monad[FC] =
    A.monad

  implicit def asyncFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], S:Async[F]):Async[FC] =
    A.async

  implicit def bracketFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], B:Bracket[F, E]):Bracket[FC, E] =
    A.bracket

  implicit def concurrentFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], C:Concurrent[F]):Concurrent[FC] =
    A.concurrent

  implicit def concurrentEffectFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], C:ConcurrentEffect[F]):ConcurrentEffect[FC] =
    A.concurrentEffect

  implicit def applicativeFailFC[F[_], FC[_], L, E, V, C](implicit A:WithContext[FC, F, L, E, V, C], AF:ApplicativeFail[F, V]):ApplicativeFail[FC, V] =
    A.applicativeFail
}

object EnvironmentLift extends EnvironmentLift