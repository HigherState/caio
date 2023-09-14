package caio.std

import caio.{Caio, OutcomeCaio}
import cats.effect.{FiberIO, OutcomeIO}
import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.unsafe.implicits.global

import scala.util.{Failure, Success, Try}

class CaioFiber[C, L, A](fiber: FiberIO[A]) extends Fiber[Caio[C, L, _], Throwable, A] {
  import CaioFiber.toOutcomeCaio

  override def cancel: Caio[C, L, Unit] =
    Caio.liftIO(fiber.cancel)

  override def join: Caio[C, L, Outcome[Caio[C, L, _], Throwable, A]] =
    Caio.liftIO(fiber.join.map(toOutcomeCaio[C, L, A]))
}

object CaioFiber {
  def apply[C, L, A](fiberIO: FiberIO[A]): CaioFiber[C, L, A] =
    new CaioFiber[C, L, A](fiberIO)

  def toOutcomeCaio[C, L, A](
    outcomeIO: OutcomeIO[A]
  ): OutcomeCaio[C, L, A] =
    outcomeIO match {
      case Outcome.Canceled() =>
        Outcome.canceled[Caio[C, L, _], Throwable, A]
      case Outcome.Errored(ex) =>
        Outcome.errored[Caio[C, L, _], Throwable, A](ex)
      case Outcome.Succeeded(io) =>
        Try(io.unsafeRunSync()) match {
          case Failure(e) =>
            Outcome.errored[Caio[C, L, _], Throwable, A](e)
          case Success(a) =>
            Outcome.succeeded(Caio.pure(a))
        }
    }
}
