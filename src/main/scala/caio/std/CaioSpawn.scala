package caio.std

import caio.{Caio, FiberCaio, FoldCaioError, FoldCaioPure, FoldCaioSuccess, OutcomeCaio}
import cats.effect.{FiberIO, IO, OutcomeIO}
import cats.effect.kernel.{Fiber, Outcome, Spawn}
import cats.effect.unsafe.implicits.global

trait CaioSpawn[C, L] extends CaioMonadCancel[C, L] with Spawn[Caio[C, L, *]] {
  def start[A](fa: Caio[C, L, A]): Caio[C, L, Fiber[Caio[C, L, *], Throwable, A]] =
    Caio.KleisliCaio[C, L, FiberCaio[C, L, A]] { case (c, ref) =>
      Caio.foldIO[C, L, A](fa, c, ref).start.map { fiber =>
        FoldCaioSuccess(c, None, fiber2Caio(fiber))
      }
    }

  def cede: Caio[C, L, Unit] =
    Caio.cede

  def racePair[A, B](
    fa: Caio[C, L, A],
    fb: Caio[C, L, B]
  ): Caio[C, L, Either[(OutcomeCaio[C, L, A], FiberCaio[C, L, B]), (FiberCaio[C, L, A], OutcomeCaio[C, L, B])]] =
    Caio
      .KleisliCaio[C, L, Either[
        ((Option[C], OutcomeCaio[C, L, A]), FiberCaio[C, L, B]),
        (FiberCaio[C, L, A], (Option[C], OutcomeCaio[C, L, B]))
      ]] { case (c, ref) =>
        val sa = Caio.foldIO[C, L, A](fa, c, ref)
        val sb = Caio.foldIO[C, L, B](fb, c, ref)

        IO.racePair(sa, sb).flatMap {
          case Left((outcomeIO @ (Outcome.Canceled() | Outcome.Errored(_)), fiberB)) =>
            ref.get.map(l =>
              FoldCaioSuccess(c, l, Left((toOutcomeCaio[A](outcomeIO).copy(_1 = Some(c)), fiber2Caio(fiberB))))
            )

          case Left((Outcome.Succeeded(io), fiberB)) =>
            io.flatMap {
              case e: FoldCaioError[C, L, _]   =>
                fiberB.cancel.map(_ => e)
              case s: FoldCaioSuccess[C, L, A] =>
                IO.pure(
                  s.map(a =>
                    Left((Some(s.c), Outcome.succeeded[Caio[C, L, *], Throwable, A](Caio.pure(a))) -> fiber2Caio(fiberB))
                  )
                )
            }

          case Right((fiberA, outcomeIO @ (Outcome.Canceled() | Outcome.Errored(_)))) =>
            ref.get.map(l =>
              FoldCaioSuccess(c, l, Right((fiber2Caio(fiberA), toOutcomeCaio[B](outcomeIO).copy(_1 = Some(c)))))
            )

          case Right((fiberA, Outcome.Succeeded(io))) =>
            io.flatMap {
              case e: FoldCaioError[C, L, _]   =>
                fiberA.cancel.map(_ => e)
              case s: FoldCaioSuccess[C, L, B] =>
                IO.pure(
                  s.map(b =>
                    Right((fiber2Caio(fiberA), (Some(s.c), Outcome.succeeded[Caio[C, L, *], Throwable, B](Caio.pure(b)))))
                  )
                )
            }
        }
      }
      .flatMap {
        case Left((tuple, fiber))  =>
          setContext(tuple).map(outcomeCaio => Left((outcomeCaio, fiber)))
        case Right((fiber, tuple)) =>
          setContext(tuple).map(outcomeCaio => Right((fiber, outcomeCaio)))
      }

  @inline private def setContext[A](tuple: (Option[C], OutcomeCaio[C, L, A])): Caio[C, L, OutcomeCaio[C, L, A]] =
    tuple match {
      case (Some(c), outcomeCaio) =>
        Caio.setContext(c).as(outcomeCaio)
      case (None, outcomeCaio)    =>
        Caio.pure(outcomeCaio)
    }

  @inline private def toOutcomeCaio[A](outcomeIO: OutcomeIO[FoldCaioPure[C, L, A]]): (Option[C], OutcomeCaio[C, L, A]) =
    outcomeIO match {
      case Outcome.Canceled()    =>
        (None, Outcome.canceled[Caio[C, L, *], Throwable, A])
      case Outcome.Errored(ex)   =>
        (None, Outcome.errored[Caio[C, L, *], Throwable, A](ex))
      case Outcome.Succeeded(io) =>
        io.unsafeRunSync() match {
          case FoldCaioError(c, _, e)   =>
            (Some(c), Outcome.errored[Caio[C, L, *], Throwable, A](e))
          case FoldCaioSuccess(c, _, a) =>
            (Some(c), Outcome.succeeded(Caio.pure(a)))
        }
    }

  @inline private def fiber2Caio[A](fiber: FiberIO[FoldCaioPure[C, L, A]]): FiberCaio[C, L, A] =
    new Fiber[Caio[C, L, *], Throwable, A] {
      final def cancel: Caio[C, L, Unit] =
        Caio.liftIO(fiber.cancel)

      final def join: Caio[C, L, OutcomeCaio[C, L, A]] =
        Caio.liftIO(fiber.join.map(toOutcomeCaio[A])).flatMap(setContext[A])
    }
}

object CaioSpawn {
  def apply[C, L]: CaioSpawn[C, L] =
    new CaioSpawn[C, L] with CaioUnique[C, L] {
      def never[A]: Caio[C, L, A] =
        Caio.never[A]
    }
}
