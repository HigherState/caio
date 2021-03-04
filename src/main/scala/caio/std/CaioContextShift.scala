package caio.std

import caio._
import cats.Monoid
import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

class CaioContextShift[C, V, L:Monoid](implicit CS:ContextShift[IO]) extends ContextShift[Caio[C, V, L, *]] {

  def evalOn[A](ec: ExecutionContext)(fa: Caio[C, V, L, A]): Caio[C, V, L, A] =
    KleisliCaio[C, V, L, A] { c =>
      CS.evalOn(ec)(Caio.foldIO(fa, c))
    }

  def shift: Caio[C, V, L, Unit] =
    IOCaio(CS.shift)
}
