package caio.std

import caio.{Caio, KleisliCaio}
import cats.{Eq, Monoid}
import cats.arrow.FunctionK

class CaioFunctionK[C1, C2, V, L: Monoid:Eq](f: C2 => C1, invF: C1 => C2)
  extends FunctionK[Caio[C1, V, L, *], Caio[C2, V, L, *]] {
  def apply[A](fa: Caio[C1, V, L, A]): Caio[C2, V, L, A] =
    KleisliCaio[C2, V, L, A] { c2 =>
      Caio.foldIO[C1, V, L, A](fa, f(c2)).contextMap(invF)
    }
}
