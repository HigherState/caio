package caio.std

import caio.Caio
import caio.mtl.InvariantAsk
import cats.CommutativeApplicative

class CaioAsk[C, L] extends InvariantAsk[Caio[C, L, *], C] {
  val applicative: CommutativeApplicative[Caio[C, L, *]] =
    new CaioApplicative[C, L]

  def ask[C1 >: C]: Caio[C, L, C1] =
    Caio.getContext

  override def reader[A](f: C => A): Caio[C, L, A] =
    Caio.getContext.map(f)
}
