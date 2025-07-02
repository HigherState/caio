package caio.std

import caio.{Caio, OutcomeCaio}
import cats.effect.{FiberIO, IO, OutcomeIO}
import cats.effect.kernel.{Fiber, Outcome}

class CaioFiber[C, L, A](fiber: FiberIO[A]) extends Fiber[Caio[C, L, _], Throwable, A] {
  import CaioFiber.toOutcomeCaio

  override def cancel: Caio[C, L, Unit] =
    Caio.liftIO(fiber.cancel)

  override def join: Caio[C, L, Outcome[Caio[C, L, _], Throwable, A]] =
    Caio.liftIO(fiber.join.flatMap(toOutcomeCaio[C, L, A]))
}

object CaioFiber {
  def apply[C, L, A](fiberIO: FiberIO[A]): CaioFiber[C, L, A] =
    new CaioFiber[C, L, A](fiberIO)

  def toOutcomeCaio[C, L, A](outcomeIO: OutcomeIO[A]): IO[OutcomeCaio[C, L, A]] =
    outcomeIO match {
      case Outcome.Canceled()    =>
        IO.pure(Outcome.canceled[Caio[C, L, _], Throwable, A])
      case Outcome.Errored(ex)   =>
        IO.pure(Outcome.errored[Caio[C, L, _], Throwable, A](ex))
      case Outcome.Succeeded(io) =>
        io.attempt.map {
          case Left(e)  =>
            Outcome.errored[Caio[C, L, _], Throwable, A](e)
          case Right(a) =>
            Outcome.succeeded(Caio.pure(a))
        }
    }
}
