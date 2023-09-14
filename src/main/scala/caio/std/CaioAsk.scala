package caio.std

import caio.Caio
import cats.CommutativeApplicative
import caio.mtl.InvariantAsk

trait CaioAsk[C, L] extends InvariantAsk[Caio[C, L, _], C] {
  def ask[C1 >: C]: Caio[C, L, C1] =
    Caio.getContext[C1]

  override def reader[A](f: C => A): Caio[C, L, A] =
    Caio.getContext.map(f)
}

object CaioAsk {
  def apply[C, L]: CaioAsk[C, L] =
    new CaioAsk[C, L] {
      val applicative: CommutativeApplicative[Caio[C, L, _]] =
        CaioApplicative[C, L]
    }
}
