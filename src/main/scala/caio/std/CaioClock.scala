package caio.std

import caio.Caio
import cats.CommutativeApplicative
import cats.effect.Clock
import scala.concurrent.duration.FiniteDuration

trait CaioClock[C, L] extends Clock[Caio[C, L, *]] {
  def monotonic: Caio[C, L, FiniteDuration] =
    Caio.monotonic

  def realTime: Caio[C, L, FiniteDuration] =
    Caio.realTime
}

object CaioClock {
  def apply[C, L]: CaioClock[C, L] =
    new CaioClock[C, L] {
      val applicative: CommutativeApplicative[Caio[C, L, *]] =
        CaioApplicative[C, L]
    }
}
