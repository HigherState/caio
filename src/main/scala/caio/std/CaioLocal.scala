package caio.std

import caio.Caio
import cats.CommutativeApplicative
import cats.mtl.Local

trait CaioLocal[C, L] extends CaioAsk[C, L] with Local[Caio[C, L, _], C] {

  def local[A](fa: Caio[C, L, A])(f: C => C): Caio[C, L, A] =
    fa.localContext(f)
}

object CaioLocal {
  def apply[C, L]: CaioLocal[C, L] =
    new CaioLocal[C, L] {
      val applicative: CommutativeApplicative[Caio[C, L, _]] =
        CaioApplicative[C, L]
    }
}
