package caio.std

import caio.Caio
import cats.arrow.FunctionK
import io.typechecked.alphabetsoup.Mixer

/**
 * FunctionK for Caio to transform the Context
 * @param f Map from context to nested context
 * @param invF Return from context to original context,
 *             Combine the resulting C1 with the initial C2
 *             Context may have been altered so we dont want to lose that change.
 * @param monoid$L$0
 * @tparam C1
 * @tparam C2
 * @tparam V
 * @tparam L
 */
class CaioFunctionK[C1, C2, L](f: C2 => C1, invF: (C1, C2) => C2) extends FunctionK[Caio[C1, L, *], Caio[C2, L, *]] {
  def apply[A](fa: Caio[C1, L, A]): Caio[C2, L, A] =
    Caio.KleisliCaio[C2, L, A] { c2 =>
      Caio.foldIO[C1, L, A](fa, f(c2)).map(_.contextMap(c1 => invF(c1, c2)))
    }
}

object CaioFunctionK {

  /**
   * Extends the Context C1 by E giving C2
   * FunctionK doesnt require concrete E as intial Left hand Side F[_] can evaluate without E
   * So should simply lift into right handside G[_]
   * @tparam C1
   * @tparam C2
   * @tparam C
   * @tparam V
   * @tparam L
   * @return
   */
  def extendBy[C1, C2, E, L](implicit
    M: Mixer[(C1, E), C2],
    I: Mixer[(C2, Unit), C1]
  ): FunctionK[Caio[C1, L, *], Caio[C2, L, *]]                                                           =
    new CaioFunctionK[C1, C2, L](c2 => I.mix(c2 -> ()), (c1, c2) => I.inject(c1, c2 -> ())._1)

  /**
   * Extends the Context C1 by E giving C2
   * FunctionK can with Provided concrete E can reduce the leftHand Side F[_] from the combined
   * Context C2, to the Simpler context used in the right handside G[_], C1
   * @param e
   * @param M
   * @param I
   * @tparam C1
   * @tparam C2
   * @tparam E
   * @tparam V
   * @tparam L
   * @return
   */
  def resolveWith[C1, C2, E, L](
    e: E
  )(implicit M: Mixer[(C1, E), C2], I: Mixer[(C2, Unit), C1]): FunctionK[Caio[C2, L, *], Caio[C1, L, *]] =
    new CaioFunctionK[C2, C1, L](c1 => M.mix(c1 -> e), (c2, _) => I.mix(c2 -> (())))
}
