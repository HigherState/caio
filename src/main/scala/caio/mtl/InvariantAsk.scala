package caio.mtl

import cats.mtl.Ask

trait InvariantAsk[F[_], E] extends Ask[F, E]

object InvariantAsk {
  def apply[F[_], E](implicit ask: InvariantAsk[F, E]): InvariantAsk[F, E] =
    ask
  
  def ask[F[_], E](implicit ask: InvariantAsk[F, E]): F[E] =
    ask.ask
}