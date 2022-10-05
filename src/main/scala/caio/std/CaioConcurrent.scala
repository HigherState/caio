package caio.std

import caio._
import cats.Monoid
import cats.effect._

class CaioConcurrent[C, L: Monoid](implicit CS: ContextShift[IO]) extends CaioAsync[C, L] with Concurrent[Caio[C, L, *]] {

  def start[A](fa: Caio[C, L, A]): Caio[C, L, Fiber[Caio[C, L, *], A]] =
    KleisliCaio[C, L, Fiber[Caio[C, L, *], A]] { c =>
      val fiberIO: IO[Fiber[IO, FoldCaioPure[C, L, A]]] =
        Caio.foldIO(fa, c).start
      fiberIO.map { fiber =>
        val cancel: IO[Unit] = fiber.cancel

        val join = fiber.join
        FoldCaioSuccess(c, Monoid[L].empty, Fiber(KleisliCaio[C, L, A](_ => join), IOCaio(cancel)))
      }
    }

  def racePair[A, B](
    fa: Caio[C, L, A],
    fb: Caio[C, L, B]
  ): Caio[C, L, Either[(A, Fiber[Caio[C, L, *], B]), (Fiber[Caio[C, L, *], A], B)]] =
    KleisliCaio[C, L, Either[(A, Fiber[Caio[C, L, *], B]), (Fiber[Caio[C, L, *], A], B)]] { c =>
      val sa = Caio.foldIO(fa, c)
      val sb = Caio.foldIO(fb, c)
      IO.racePair(sa, sb).flatMap {
        case Left((e: FoldCaioError[C, L, _], fiberB)) =>
          fiberB.cancel.map(_ => e)

        case Left((s: FoldCaioSuccess[C, L, A], fiberB)) =>
          IO(s.map(a => Left(a -> fiber2Caio(fiberB))))

        case Right((fiberA, e: FoldCaioError[C, L, _]))  =>
          fiberA.cancel.map(_ => e)

        case Right((fiberA, s: FoldCaioSuccess[C, L, B])) =>
          IO(s.map(b => Right(fiber2Caio(fiberA) -> b)))
      }
    }

  private def fiber2Caio[A](fiber: Fiber[IO, FoldCaioPure[C, L, A]]): Fiber[Caio[C, L, *], A] = {
    val cancel: CancelToken[IO] = fiber.cancel
    val join                    = KleisliCaio[C, L, A](_ => fiber.join)
    Fiber(join, IOCaio(cancel))
  }
}
