package caio.mtl

import cats.{Applicative, ApplicativeError, Functor, Monad, MonadError}
import cats.effect._
import cats.mtl._
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait Context[F[_], C, L, E, V] {

  def apply[C2](implicit M:Mixer[(C, C2), C2], EV: C =:!= C2):ContextApplicator[F, L, E, V, C, C2]
}


trait ContextApplicator[F[_], L, E, V, Init, Additional] {

  type FC[A]

  type C = (Init, Additional)

  def applicativeAsk:ApplicativeAsk[FC, C]

  def context:Context[FC, C, L, E, V]

  def applicative(implicit A:Applicative[F]):Applicative[FC] =
    A.asInstanceOf[Applicative[FC]]

  def functor(implicit F:Functor[F]):Functor[FC] =
    F.asInstanceOf[Functor[FC]]

  def applicativeError(implicit AE:ApplicativeError[F, E]):ApplicativeError[FC, E] =
    AE.asInstanceOf[ApplicativeError[FC, E]]

  def applicativeFail(implicit AF:ApplicativeFail[F, V]):ApplicativeFail[FC, V] =
    AF.asInstanceOf[ApplicativeFail[FC, V]]

  def monadError(implicit ME:MonadError[F, E]):MonadError[FC, E] =
    ME.asInstanceOf[MonadError[FC, E]]

  def monad(implicit M:Monad[F]):Monad[FC] =
    M.asInstanceOf[Monad[FC]]

  def sync(implicit S:Sync[F]):Sync[FC] =
    S.asInstanceOf[Sync[FC]]

  def async(implicit A:Async[F]):Async[FC] =
    A.asInstanceOf[Async[FC]]

  def bracket(implicit A:Bracket[F, E]):Bracket[FC, E] =
    A.asInstanceOf[Bracket[FC, E]]

  def liftIO(implicit L:LiftIO[F]):LiftIO[FC] =
    L.asInstanceOf[LiftIO[FC]]

  def concurrent(implicit C:Concurrent[F]):Concurrent[FC] =
    C.asInstanceOf[Concurrent[FC]]

  def concurrentEffect(implicit C:ConcurrentEffect[F]):ConcurrentEffect[FC] = ???

  def apply[A](c:Additional)(f:FC[A]):F[A]
}

trait EnvironmentLift {

  implicit def toAux[F[_], L, E, V, Init, Additional](implicit E:ContextApplicator[F, L, E, V, Init, Additional]): WithContext[E.FC, F, L, E, V, Init, Additional] =
    E.asInstanceOf[WithContext[E.FC, F, L, E, V, Init, Additional]]

  implicit def context[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional]):Context[FC, (Init, Additional), L, E, V] =
    A.context

  implicit def applicativeAskFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional]):ApplicativeAsk[FC, (Init, Additional)] =
    A.applicativeAsk

//  implicit def monadStateFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional]):MonadState[FC, (Init, Additional)] =
//    A.monadState

  implicit def applicativeErrorFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], AE:ApplicativeError[F, E]):ApplicativeError[FC, E] =
    A.applicativeError

  implicit def monadErrorFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], ME:MonadError[F, E]):MonadError[FC, E] =
   A.monadError

  implicit def liftIOFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], L:LiftIO[F]):LiftIO[FC] =
    A.liftIO

  implicit def syncFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], S:Sync[F]):Sync[FC] =
    A.sync

  implicit def monadFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], M:Monad[F]):Monad[FC] =
    A.monad

  implicit def asyncFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], S:Async[F]):Async[FC] =
    A.async

  implicit def bracketFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], B:Bracket[F, E]):Bracket[FC, E] =
    A.bracket

  implicit def concurrentFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], C:Concurrent[F]):Concurrent[FC] =
    A.concurrent

  implicit def concurrentEffectFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], C:ConcurrentEffect[F]):ConcurrentEffect[FC] =
    A.concurrentEffect

  implicit def applicativeFailFC[F[_], FC[_], L, E, V, Init, Additional](implicit A:WithContext[FC, F, L, E, V, Init, Additional], AF:ApplicativeFail[F, V]):ApplicativeFail[FC, V] =
    A.applicativeFail
}

object EnvironmentLift extends EnvironmentLift
