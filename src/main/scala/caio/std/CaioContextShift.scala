package caio.std

import caio._
import cats.Monoid
import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

class CaioContextShift[C, L: Monoid](implicit CS: ContextShift[IO]) extends ContextShift[Caio[C, L, *]] {

  def evalOn[A](ec: ExecutionContext)(fa: Caio[C, L, A]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      CS.evalOn(ec)(Caio.foldIO(fa, c))
    }

  def shift: Caio[C, L, Unit] =
    Caio.liftIO(CS.shift)
}
