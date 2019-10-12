package caio.mtl

import cats.{Applicative, Monad}
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait ContextProjector {

  implicit def projectMonadState[M[_], A]
    (implicit MSP: StateProjection[M, A]): MonadState[M, A] =
    MSP.MS

  implicit def projectApplicativeAsk[M[_], A]
    (implicit AP: AskProjection[M, A]): ApplicativeAsk[M, A] =
    AP.A



  implicit def extenderAskProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):AskProjection[M, B] =
    AskProjection{
      new ApplicativeAsk[M, B] {

        val applicative: Applicative[M] =
          E.applicativeAsk.applicative

        def ask: M[B] =
          applicative.map(E.applicativeAsk.ask)(M.mix)

        def reader[C](f: B => C): M[C] =
          E.applicativeAsk.reader(a => f(M.mix(a)))
      }
    }

  implicit def askAskProjection[M[_], A, B]
    (implicit AA:ApplicativeAsk[M, A], M:Mixer[A, B], EV: A =:!= B):AskProjection[M, B] =
    AskProjection{
      new ApplicativeAsk[M, B] {
        val applicative: Applicative[M] =
          AA.applicative

        def ask: M[B] =
          applicative.map(AA.ask)(M.mix)

        def reader[C](f: B => C): M[C] =
          AA.reader[C](a => f(M.mix(a)))
      }
    }



  implicit def stateStateProjection[M[_], A, B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    StateProjection{
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

  implicit def extenderStateProjection[M[_], A, B]
    (implicit E:Extender[M, A], M:Mixer[A, B]):StateProjection[M, B] =
    StateProjection{
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

}



case class AskProjection[M[_], A](A:ApplicativeAsk[M, A]) extends AnyVal
case class StateProjection[M[_], A](MS:MonadState[M, A]) extends AnyVal

object ContextProjector extends ContextProjector


