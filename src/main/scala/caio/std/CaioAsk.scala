package caio.std

import caio.Caio
import caio.mtl.InvariantAsk
import cats.CommutativeApplicative

class CaioAsk[C, V, L] extends InvariantAsk[Caio[C, V, L, *], C] {
  val applicative: CommutativeApplicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def ask[C1 >: C]: Caio[C, V, L, C1] =
    Caio.getContext

  override def reader[A](f: C => A): Caio[C, V, L, A] =
    Caio.getContext.map(f)
}
