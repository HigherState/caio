package caio.mtl

import cats.{Applicative, Monad}
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait ArgsProjector {

  implicit def projectMonadState[M[_], A, B]
    (implicit MS: MonadState[M, A], EV: A =:!= B, M: Mixer[A, B]): MonadState[M, B] =
    new MonadState[M, B] {
      val monad: Monad[M] = MS.monad

      def get: M[B] =
        monad.map(MS.get)(M.mix)

      def set(b: B): M[Unit] =
        MS.modify(a => M.inject(b, a))

      def inspect[B2](f: B => B2): M[B2] =
        MS.inspect (a => f(M.mix(a)))

      def modify(f: B => B): M[Unit] =
        MS.modify(a => M.modify(f)(a))
    }

  implicit def projectionApplicativeAsk[M[_], A]
    (implicit eitherAndProjection: EitherAndProjection[M, A]): ApplicativeAsk[M, A] = null

  implicit def askProjection[M[_], A, B]
    (implicit AA:ApplicativeAsk[M, A], M:Mixer[A, B], EV: A =:!= B):EitherAndProjection[M, B] =
    AskProjection(AA, M)

  implicit def stateProjection[M[_], A, B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B], EV: A =:!= B):EitherAndProjection[M, B] =
    StateProjection(MS, M)

//TODO stop from causing implicit diverging
//  implicit def bothProjection[M[_], A1, A2, B]
//    (implicit AA:ApplicativeAsk[M, A1], MS:MonadState[M, A2], M:Mixer[(A1, A2), B], EV1: A1 =:!= B, EV2: A2 =:!= B):EitherAndProjection[M, B] =
//    BothProjection(AA, MS, M)
}

sealed trait EitherAndProjection[M[_], A] {
  def apply():ApplicativeAsk[M, A]
}
case class AskProjection[M[_], A, B](AA:ApplicativeAsk[M, A], M:Mixer[A, B]) extends EitherAndProjection[M, B] {
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
case class StateProjection[M[_], A, B](MS:MonadState[M, A], M:Mixer[A, B]) extends EitherAndProjection[M, B] {
  def apply():ApplicativeAsk[M, B] =
    new ApplicativeAsk[M, B] {

      val applicative: Applicative[M] =
        MS.monad

      def ask: M[B] =
        applicative.map(MS.get)(M.mix)

      def reader[C](f: B => C): M[C] =
        MS.inspect(a => f(M.mix(a)))
    }
}
case class BothProjection[M[_], A1, A2, B](AA:ApplicativeAsk[M, A1], MS:MonadState[M, A2], M:Mixer[(A1, A2), B]) extends EitherAndProjection[M, B] {
  def apply():ApplicativeAsk[M, B] =
    new ApplicativeAsk[M, B] {

      val applicative: Applicative[M] =
        AA.applicative

      def ask: M[B] =
        MS.monad.flatMap(AA.ask){a1 =>
          MS.monad.map{MS.get}{a2 =>
            M.mix(a1 -> a2)
          }
        }

      def reader[C](f: B => C): M[C] =
        MS.monad.map(ask)(f)
    }
}


object ArgsProjector extends ArgsProjector
