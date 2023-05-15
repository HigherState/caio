package caio.std

import caio.{Caio, FiberCaio, FoldCaioError, FoldCaioPure, FoldCaioSuccess, OutcomeCaio}
import cats.Monoid
import cats.effect.{FiberIO, IO, OutcomeIO}
import cats.effect.kernel.{Fiber, Outcome, Spawn}
import cats.effect.unsafe.implicits.global

trait CaioSpawn[C, L] extends CaioMonadCancel[C, L] with Spawn[Caio[C, L, *]] {
  import CaioSpawn.{fiber2Caio, toOutcomeCaio}

  def start[A](fa: Caio[C, L, A]): Caio[C, L, Fiber[Caio[C, L, *], Throwable, A]] =
    Caio.KleisliCaio[C, L, FiberCaio[C, L, A]] { c =>
      Caio.foldIO[C, L, A](fa, c).start.map { fiber =>
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
        (OutcomeCaio[C, L, A], FiberCaio[C, L, B]),
        (FiberCaio[C, L, A], OutcomeCaio[C, L, B])
      ]] { case c =>
        val sa = Caio.foldIO[C, L, A](fa, c)
        val sb = Caio.foldIO[C, L, B](fb, c)

        IO.racePair(sa, sb).flatMap {
          case Left((outcomeIO @ (Outcome.Canceled() | Outcome.Errored(_)), fiberB)) =>
            IO.pure(FoldCaioSuccess(c, None, Left((toOutcomeCaio[C, L, A](outcomeIO)._3, fiber2Caio(fiberB)))))

          case Left((Outcome.Succeeded(io), fiberB)) =>
            io.flatMap {
              case e: FoldCaioError[C, L, _]   =>
                fiberB.cancel.map(_ => e)
              case s: FoldCaioSuccess[C, L, A] =>
                IO.pure(
                  s.map(a =>
                    Left(
                      Outcome.succeeded[Caio[C, L, *], Throwable, A](Caio.pure(a)) -> fiber2Caio(
                        fiberB
                      )
                    )
                  )
                )
            }

          case Right((fiberA, outcomeIO @ (Outcome.Canceled() | Outcome.Errored(_)))) =>
            IO.pure(FoldCaioSuccess(c, None, Right((fiber2Caio(fiberA), toOutcomeCaio[C, L, B](outcomeIO)._3))))

          case Right((fiberA, Outcome.Succeeded(io))) =>
            io.flatMap {
              case e: FoldCaioError[C, L, _]   =>
                fiberA.cancel.map(_ => e)
              case s: FoldCaioSuccess[C, L, B] =>
                IO.pure(
                  s.map(b =>
                    Right(
                      (fiber2Caio(fiberA), Outcome.succeeded[Caio[C, L, *], Throwable, B](Caio.pure(b)))
                    )
                  )
                )
            }
        }
      }
}

object CaioSpawn {
  def apply[C, L]: CaioSpawn[C, L] =
    new CaioSpawn[C, L] with CaioUnique[C, L] {
      def never[A]: Caio[C, L, A] =
        Caio.never[A]
    }

  @inline private def setLogsAndContext[C, L, A](
    tuple: (Option[C], Option[(L, Monoid[L])], OutcomeCaio[C, L, A])
  ): Caio[C, L, OutcomeCaio[C, L, A]] =
    tuple match {
      case (Some(c), Some((l, monoid)), outcomeCaio) =>
        Caio.setContext(c) *> Caio.tell(l)(monoid).as(outcomeCaio)
      case (Some(c), None, outcomeCaio) =>
        Caio.setContext(c).as(outcomeCaio)
      case (None, Some((l, monoid)), outcomeCaio) =>
        Caio.tell(l)(monoid).as(outcomeCaio)
      case (None, None, outcomeCaio) =>
        Caio.pure(outcomeCaio)
    }

  @inline private def toOutcomeCaio[C, L, A](
    outcomeIO: OutcomeIO[FoldCaioPure[C, L, A]]
  ): (Option[C], Option[(L, Monoid[L])], OutcomeCaio[C, L, A]) =
    outcomeIO match {
      case Outcome.Canceled() =>
        (None, None, Outcome.canceled[Caio[C, L, *], Throwable, A])
      case Outcome.Errored(ex) =>
        (None, None, Outcome.errored[Caio[C, L, *], Throwable, A](ex))
      case Outcome.Succeeded(io) =>
        io.unsafeRunSync() match {
          case FoldCaioError(c, l, e) =>
            (Some(c), l, Outcome.errored[Caio[C, L, *], Throwable, A](e))
          case FoldCaioSuccess(c, l, a) =>
            (Some(c), l, Outcome.succeeded(Caio.pure(a)))
        }
    }

  def fiber2Caio[C, L, A](fiber: FiberIO[FoldCaioPure[C, L, A]]): FiberCaio[C, L, A] =
    new Fiber[Caio[C, L, *], Throwable, A] {
      final def cancel: Caio[C, L, Unit] =
        Caio.liftIO(fiber.cancel)

      final def join: Caio[C, L, OutcomeCaio[C, L, A]] =
        Caio.liftIO(fiber.join.map(toOutcomeCaio[C, L, A])).flatMap(setLogsAndContext[C, L, A])
    }
}
