package caio.std

import caio.Caio
import cats.effect.Temporal
import scala.concurrent.duration.FiniteDuration

trait CaioTemporal[C, L] extends CaioConcurrent[C, L] with CaioClock[C, L] with Temporal[Caio[C, L, *]] {
  def sleep(duration: FiniteDuration): Caio[C, L, Unit] =
    Caio.sleep(duration)
}

object CaioTemporal {
  def apply[C, L]: CaioTemporal[C, L] =
    new CaioTemporal[C, L] with CaioUnique[C, L] {
      def never[A]: Caio[C, L, A] =
        Caio.never[A]
    }
}
