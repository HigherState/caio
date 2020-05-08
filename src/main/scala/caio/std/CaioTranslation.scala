package caio.std

import caio.Caio
import cats.{Eq, Monoid, ~>}
import io.typechecked.alphabetsoup.Mixer

class CaioTranslation[E1, E2, V, L: Monoid:Eq, C](
   implicit M: Mixer[(E1, C), E2],
   I: Mixer[(E2, Unit), E1],
 ) {

  val forwardsFunctionK: Caio[E1, V, L, *] ~> Caio[E2, V, L, *] =
    new CaioFunctionK[E1, E2, V, L](p => I.mix(p -> (())))

  def backwardsFunctionK(c: C): Caio[E2, V, L, *] ~> Caio[E1, V, L, *] =
    new CaioFunctionK[E2, E1, V, L](p => M.mix(p -> c))

}