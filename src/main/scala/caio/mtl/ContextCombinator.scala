package caio.mtl

import cats.Applicative
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

//Cannot be combined with ContextProjector, Implicit divergence
trait ContextCombinator {

  implicit def combinateAsk[M[_], A](implicit AC: AskCombinator[M, A]):ApplicativeAsk[M, A] =
    AC.apply()

  implicit def askStateCombinator[M[_], A1, A2, B]
    (implicit AA:ApplicativeAsk[M, A1], MS:MonadState[M, A2], M:Mixer[(A1, A2), B]):AskCombinator[M, B] =
    AskStateCombinator(AA, MS, M)


  implicit def stateCombinator[M[_], A,  B]
    (implicit MS:MonadState[M, A], M:Mixer[A, B]):AskCombinator[M, B] =
    StateCombinator(MS, M)

}

sealed trait AskCombinator[M[_], A] {
  def apply():ApplicativeAsk[M, A]
}
case class StateCombinator[M[_], A, B](MS:MonadState[M, A], M:Mixer[A, B]) extends AskCombinator[M, B] {
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
case class AskStateCombinator[M[_], A1, A2, B](AA:ApplicativeAsk[M, A1], MS:MonadState[M, A2], M:Mixer[(A1, A2), B]) extends AskCombinator[M, B] {
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

object ContextCombinator extends ContextCombinator