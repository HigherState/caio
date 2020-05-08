package caio.std

import caio.{Caio, FoldCaioIO, KleisliCaio}
import cats.{Eq, Monoid}
import cats.arrow.FunctionK

/**
 * FunctionK for Caio to transform the Context
 * @param f Map from context to nested context
 * @param invF Return from context to original context,
 *             context may have been altered so we dont want to lose that change.
 * @param monoid$L$0
 * @param eq$L$1
 * @tparam C1
 * @tparam C2
 * @tparam V
 * @tparam L
 */
class CaioContextFunctionK[C1, C2, V, L: Monoid:Eq](f: C2 => C1, invF: C1 => C2)
  extends FunctionK[Caio[C1, V, L, *], Caio[C2, V, L, *]] {
  def apply[A](fa: Caio[C1, V, L, A]): Caio[C2, V, L, A] =
    KleisliCaio[C2, V, L, A] { c2 =>
      FoldCaioIO(Caio.foldIO[C1, V, L, A](fa, f(c2)).map(_.contextMap(invF)))
    }

  val invert:FunctionK[Caio[C2, V, L, *], Caio[C1, V, L, *]] =
    new FunctionK[Caio[C2, V, L, *], Caio[C1, V, L, *]]{

    def apply[A](fa: Caio[C2, V, L, A]): Caio[C1, V, L, A] =
      KleisliCaio[C1, V, L, A] { c1 =>
        FoldCaioIO(Caio.foldIO[C2, V, L, A](fa, invF(c1)).map(_.contextMap(f)))
      }
    }
}
