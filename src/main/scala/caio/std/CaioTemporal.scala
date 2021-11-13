package caio.std

import caio.{Caio, IOCaio}
import cats.effect.{IO, Temporal}
import scala.concurrent.duration.FiniteDuration

trait CaioTemporal[C, L] extends CaioConcurrent[C, L] with CaioClock[C, L] with Temporal[Caio[C, L, *]] {
  def sleep(time: FiniteDuration): Caio[C, L, Unit] =
    IOCaio(IO.sleep(time))
}
