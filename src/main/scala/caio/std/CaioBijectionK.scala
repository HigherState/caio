package caio.std

import caio.{<~>, Caio}
import cats.~>
import cats.arrow.FunctionK

//TODO review under necessary change to FunctionK as not all pieces adding up here
/**
 * FunctionK for Caio to transform the Context
 * @param f Map from context to nested context
 * @param invF Return from context to original context,
 *             context may have been altered so we dont want to lose that change.
 * @param monoid$L$0
 * @tparam C1
 * @tparam C2
 * @tparam V
 * @tparam L
 */
class CaioBijectionK[C1, C2, L](f: C2 => C1, invF: C1 => C2) extends (Caio[C1, L, *] <~> Caio[C2, L, *]) {
  def apply[A](fa: Caio[C1, L, A]): Caio[C2, L, A] =
    Caio.KleisliCaio[C2, L, A] { c2 =>
      Caio.foldIO[C1, L, A](fa, f(c2)).map(_.contextMap(invF))
    }

  def unapply[A](fa: Caio[C2, L, A]): Caio[C1, L, A] =
    Caio.KleisliCaio[C1, L, A] { c1 =>
      Caio.foldIO[C2, L, A](fa, invF(c1)).map(_.contextMap(f))
    }

  def invert: Caio[C2, L, *] ~> Caio[C1, L, *] =
    new FunctionK[Caio[C2, L, *], Caio[C1, L, *]] {
      def apply[A](fa: Caio[C2, L, A]): Caio[C1, L, A] =
        Caio.KleisliCaio[C1, L, A] { c1 =>
          Caio.foldIO[C2, L, A](fa, invF(c1)).map(_.contextMap(f))
        }
    }
}
