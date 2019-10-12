package caio.mtl

import cats.{Applicative, Monad}
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait ContextProjector {

  implicit def projectMonadState[M[_], A]
    (implicit stateProjection: StateProjection[M, A]): MonadState[M, A] =
    stateProjection.apply()

  implicit def projectApplicativeAsk[M[_], A]
    (implicit askProjection: AskProjection[M, A]): ApplicativeAsk[M, A] =
    askProjection.apply()



  implicit def extenderAskProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):AskProjection[M, B] =
    ExtenderAskProjection(E, M)

  implicit def askAskProjection[M[_], A, B]
    (implicit AA:ApplicativeAsk[M, A], M:Mixer[A, B], EV: A =:!= B):AskProjection[M, B] =
    AskAskProjection(AA, M)



  implicit def stateStateProjection[M[_], A, B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    StateStateProjection(MS, M)

  implicit def extenderStateProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    ExtenderStateProjection(E, M)

}



sealed trait AskProjection[M[_], A] {
  def apply():ApplicativeAsk[M, A]
}
case class AskAskProjection[M[_], A, B](AA:ApplicativeAsk[M, A], M:Mixer[A, B]) extends AskProjection[M, B] {
  def apply():ApplicativeAsk[M, B] =
    new ApplicativeAsk[M, B] {
      val applicative: Applicative[M] =
        AA.applicative

      def ask: M[B] =
        applicative.map(AA.ask)(M.mix)

      def reader[C](f: B => C): M[C] =
        AA.reader[C](a => f(M.mix(a)))
    }
}

case class ExtenderAskProjection[M[_], A, B](E:Extender[M, A], M:Mixer[A, B]) extends AskProjection[M, B] {
  def apply():ApplicativeAsk[M, B] =
    new ApplicativeAsk[M, B] {

      val applicative: Applicative[M] =
        E.applicativeAsk.applicative

      def ask: M[B] =
        applicative.map(E.applicativeAsk.ask)(M.mix)

      def reader[C](f: B => C): M[C] =
        E.applicativeAsk.reader(a => f(M.mix(a)))
    }
}

sealed trait StateProjection[M[_], A] {
  def apply():MonadState[M, A]
}
case class StateStateProjection[M[_], A, B](MS:MonadState[M, A], M:Mixer[A, B]) extends StateProjection[M, B] {
  def apply():MonadState[M, B] =
    new MonadState[M, B] {
      val monad: Monad[M] =
        MS.monad

      def get: M[B] =
        monad.map(MS.get)(M.mix)

      def set(b: B): M[Unit] =
        MS.modify(a => M.inject(b, a))

      def inspect[B2](f: B => B2): M[B2] =
        MS.inspect (a => f(M.mix(a)))

      def modify(f: B => B): M[Unit] =
        MS.modify(a => M.modify(f)(a))
    }
}
case class ExtenderStateProjection[M[_], A, B](E:Extender[M, A], M:Mixer[A, B]) extends StateProjection[M, B] {
  def apply():MonadState[M, B] =
    new MonadState[M, B] {
      val monad: Monad[M] =
        E.monadState.monad

      def get: M[B] =
        monad.map(E.monadState.get)(M.mix)

      def set(b: B): M[Unit] =
        E.monadState.modify(a => M.inject(b, a))

      def inspect[B2](f: B => B2): M[B2] =
        E.monadState.inspect (a => f(M.mix(a)))

      def modify(f: B => B): M[Unit] =
        E.monadState.modify(a => M.modify(f)(a))
    }
}

object ContextProjector extends ContextProjector


