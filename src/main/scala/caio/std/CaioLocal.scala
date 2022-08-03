package caio.std

import caio.Caio
import cats.mtl.Local

class CaioLocal[C, L] extends CaioAsk[C, L] with Local[Caio[C, L, *], C] {
  def local[A](fa: Caio[C, L, A])(f: C => C): Caio[C, L, A] =
    fa.localContext(f)
}
