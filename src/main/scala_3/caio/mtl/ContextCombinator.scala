package caio.mtl

import cats.Applicative
import cats.mtl.Stateful
import io.typechecked.alphabetsoup.Mixer

//Cannot be combined with ContextProjector, Implicit divergence
trait ContextCombinator {

  implicit def combinatorAsk[M[_], A](implicit AC: AskCombinator[M, A]): InvariantAsk[M, A] =
    AC.A

  implicit def askStateCombinator[M[_], A1, A2, B](implicit
    AA: InvariantAsk[M, A1],
    MS: Stateful[M, A2],
    M: Mixer[(A2, A1), B]
  ): AskCombinator[M, B] =
    AskCombinator {
      new InvariantAsk[M, B] {

        val applicative: Applicative[M] =
          AA.applicative

        def ask[B2 >: B]: M[B2] =
          MS.monad.flatMap(AA.ask) { a1 =>
            MS.monad.map(MS.get) { a2 =>
              M.mix(a2 -> a1)
            }
          }

        override def reader[C](f: B => C): M[C] =
          MS.monad.map(ask)(f)
      }
    }

  implicit def stateCombinator[M[_], A, B](implicit MS: Stateful[M, A], M: Mixer[A, B]): AskCombinator[M, B] =
    AskCombinator {
      new InvariantAsk[M, B] {
        val applicative: Applicative[M] =
          MS.monad

        def ask[B2 >: B]: M[B2] =
          applicative.map(MS.get)(M.mix)

        override def reader[C](f: B => C): M[C] =
          MS.inspect(a => f(M.mix(a)))
      }
    }

}

case class AskCombinator[M[_], A](A: InvariantAsk[M, A]) extends AnyVal

object ContextCombinator extends ContextCombinator
