package caio.std

import caio._
import cats.Monoid
import cats.effect.{CancelToken, Concurrent, ContextShift, Fiber, IO}

class CaioConcurrent[C, V, L:Monoid](implicit CS:ContextShift[IO]) extends CaioAsync[C, V, L] with Concurrent[Caio[C, V, L, *]] {

  def start[A](fa: Caio[C, V, L, A]): Caio[C, V, L, Fiber[Caio[C, V, L, *], A]] =
    KleisliCaio { c =>
      FoldCaioIO {
        val fiberIO: IO[Fiber[IO, FoldCaioPure[C, V, L, A]]] =
          Caio.foldIO(fa, c).start
        fiberIO.map { fiber =>
          val cancel: IO[Unit] = fiber.cancel

          val join = fiber.join
          FoldCaioSuccess(c, Monoid[L].empty, Fiber(
            KleisliCaio(_ => FoldCaioIO(join)),
            IOCaio(cancel)
          ))
        }
      }
    }

  def racePair[A, B](fa: Caio[C, V, L, A], fb: Caio[C, V, L, B]): Caio[C, V, L, Either[(A, Fiber[Caio[C, V, L, *], B]), (Fiber[Caio[C, V, L, *], A], B)]] =
    KleisliCaio { c =>

      val sa = IO.suspend(Caio.foldIO(fa, c))
      val sb = IO.suspend(Caio.foldIO(fb, c))
      IOResult {
        IO.racePair(sa, sb).flatMap {

          case Left((e: ErrorResult[C, V, L, A], fiberB)) =>
            fiberB.cancel.map(_ => e.shiftValue)

          case Left((s: SuccessResult[C, V, L, A], fiberB)) =>
            IO(s.map(a => Left(a -> fiber2Caio(fiberB))))

          case Right((fiberA, e: ErrorResult[C, V, L, B])) =>
            fiberA.cancel.map(_ => e.shiftValue)

          case Right((fiberA, s: SuccessResult[C, V, L, B])) =>
            IO(s.map(b => Right(fiber2Caio(fiberA) -> b)))
        }
      }
    }

  private def fiber2Caio[A](fiber: Fiber[IO, PureResult[C, V, L, A]]): Fiber[Caio[C, V, L, *], A] = {
    val cancel: CancelToken[IO] = fiber.cancel
    val join = CaioKleisli[C, V, L, A](_ => IOResult(fiber.join))
    Fiber(join, liftIO(cancel))
  }
}
