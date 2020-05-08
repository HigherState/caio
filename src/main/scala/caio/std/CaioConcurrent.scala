package caio.std

import caio._
import cats.{Eq, Monoid}
import cats.effect.{CancelToken, Concurrent, ContextShift, Fiber, IO}

class CaioConcurrent[C, V, L:Monoid:Eq](implicit CS:ContextShift[IO]) extends CaioAsync[C, V, L] with Concurrent[Caio[C, V, L, *]] {

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
      FoldCaioIO {
        IO.racePair(sa, sb).flatMap {

          case Left((e: FoldCaioError[C, V, L, A], fiberB)) =>
            fiberB.cancel.map(_ => e)

          case Left((f: FoldCaioFailure[C, V, L, A], fiberB)) =>
            fiberB.cancel.map(_ => f)

          case Left((s: FoldCaioSuccess[C, V, L, A], fiberB)) =>
            IO(s.map(a => Left(a -> fiber2Caio(fiberB))))

          case Right((fiberA, e: FoldCaioError[C, V, L, A])) =>
            fiberA.cancel.map(_ => e)

          case Right((fiberA, f: FoldCaioFailure[C, V, L, A])) =>
            fiberA.cancel.map(_ => f)

          case Right((fiberA, s: FoldCaioSuccess[C, V, L, B])) =>
            IO(s.map(b => Right(fiber2Caio(fiberA) -> b)))
        }
      }
    }

  private def fiber2Caio[A](fiber: Fiber[IO, FoldCaioPure[C, V, L, A]]): Fiber[Caio[C, V, L, *], A] = {
    val cancel: CancelToken[IO] = fiber.cancel
    val join = KleisliCaio[C, V, L, A](_ => FoldCaioIO(fiber.join))
    Fiber(join, IOCaio(cancel))
  }
}
