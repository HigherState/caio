package caio.std

import caio.Caio
import cats.mtl.Local

class CaioLocal[C, V, L] extends CaioAsk[C, V, L] with Local[Caio[C, V, L, *], C] {
  def local[A](fa: Caio[C, V, L, A])(f: C => C): Caio[C, V, L, A] =
    fa.localContext(f)
}