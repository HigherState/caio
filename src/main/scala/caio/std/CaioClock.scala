package caio.std

import caio.{Caio, IOCaio}
import cats.effect.{Clock, IO}
import scala.concurrent.duration.FiniteDuration

trait CaioClock[C, L] extends Clock[Caio[C, L, *]] {

  def monotonic: Caio[C, L, FiniteDuration] =
    IOCaio(IO.monotonic)

  def realTime: Caio[C, L, FiniteDuration] =
    IOCaio(IO.realTime)
}
