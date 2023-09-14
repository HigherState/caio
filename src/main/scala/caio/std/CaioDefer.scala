package caio.std

import caio.Caio
import cats.Defer

trait CaioDefer[C, L] extends Defer[Caio[C, L, _]] {
  def defer[A](fa: => Caio[C, L, A]): Caio[C, L, A] =
    Caio.defer(fa)
}

object CaioDefer {
  def apply[C, L]: CaioDefer[C, L] =
    new CaioDefer[C, L] {}
}
