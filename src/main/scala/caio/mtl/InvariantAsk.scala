package caio.mtl

import cats.mtl.Ask
import cats.Applicative

trait InvariantAsk[F[_], E] extends Ask[F, E]

object InvariantAsk {
  def apply[F[_], E](implicit ask: InvariantAsk[F, E]): InvariantAsk[F, E] =
    ask

  def ask[F[_], E](implicit ask: InvariantAsk[F, E]): F[E] =
    ask.ask

  def const[F[_], E](const: E)(implicit A: Applicative[F]): InvariantAsk[F, E] =
    new InvariantAsk[F, E] {
      val applicative: Applicative[F] = A

      def ask[E2 >: E]: F[E2] = A.pure(const)
    }

  def map[F[_], E, E1](f: E => E1)(implicit IA: InvariantAsk[F, E]): InvariantAsk[F, E1] =
    new InvariantAsk[F, E1] {
      val applicative: Applicative[F] = IA.applicative

      def ask[E2 >: E1]: F[E2] =
        IA.applicative.map(IA.ask)(f)
    }
}
