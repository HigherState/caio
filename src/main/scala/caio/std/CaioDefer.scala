package caio.std

import caio.{Caio, KleisliCaio}
import cats.Defer

trait CaioDefer[C,L] extends CaioMonoid[L] with Defer[Caio[C, L, *]] {

  def defer[A](fa: => Caio[C, L, A]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      Caio.foldIO(fa, c)
    }
}
