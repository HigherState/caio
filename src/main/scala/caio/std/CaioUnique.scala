package caio.std

import caio.Caio
import cats.CommutativeApplicative
import cats.effect.Unique

trait CaioUnique[C, L] extends Unique[Caio[C, L, *]] {
  def unique: Caio[C, L, Unique.Token] =
    Caio.unique
}

object CaioUnique {
  def apply[C, L]: CaioUnique[C, L] =
    new CaioUnique[C, L] {
      val applicative: CommutativeApplicative[Caio[C, L, *]] =
        CaioApplicative[C, L]
    }
}
