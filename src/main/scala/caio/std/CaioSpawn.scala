package caio.std

import caio.{Caio, FoldCaioError, FoldCaioPure, FoldCaioSuccess, IOCaio, KleisliCaio}
import cats.Monoid
import cats.effect.{Fiber, IO, Spawn}
import cats.effect.kernel.Outcome

trait CaioSpawn[C, L] extends CaioMonadCancel[C, L] with CaioUnique[C, L] with CaioMonoid[L] with Spawn[Caio[C, L, *]] {

  def start[A](fa: Caio[C, L, A]): Caio[C, L, Fiber[Caio[C, L, *], Throwable, A]] =
    KleisliCaio[C, L, Fiber[Caio[C, L, *], Throwable, A]] { c =>
      val fiberIO: IO[Fiber[IO, Throwable, FoldCaioPure[C, L, A]]] =
        Caio.foldIO(fa, c).start
      fiberIO.map { fiber =>
        val cancel: IO[Unit] = fiber.cancel

        val join = fiber.join
        FoldCaioSuccess(c, Monoid[L].empty, join.to)
      }
    }

  def never[A]: Caio[C, L, A] = ???

  def cede: Caio[C, L, Unit] = ???

  def racePair[A, B](fa: Caio[C, L, A], fb: Caio[C, L, B]): Caio[C, L, Either[(Outcome[Caio[C, L, *], Throwable, A], Fiber[Caio[C, L, *], Throwable, B]), (Fiber[Caio[C, L, *], Throwable, A], Outcome[Caio[C, L, *], Throwable, B])]] =
    KleisliCaio[C, L, Either[(A, Fiber[Caio[C, L, *], B]), (Fiber[Caio[C, L, *], A], B)]] { c =>
      val sa = Caio.foldIO(fa, c)
      val sb = Caio.foldIO(fb, c)
      IO.racePair(sa, sb).flatMap {
        case Left((e: FoldCaioError[C, L, _], fiberB)) =>
          fiberB.cancel.map(_ => e)

        case Left((f: FoldCaioFailure[C, L, _], fiberB)) =>
          fiberB.cancel.map(_ => f)

        case Left((s: FoldCaioSuccess[C, L, A], fiberB)) =>
          IO(s.map(a => Left(a -> fiber2Caio(fiberB))))

        case Right((fiberA, e: FoldCaioError[C, L, _]))  =>
          fiberA.cancel.map(_ => e)

        case Right((fiberA, f: FoldCaioFailure[C, L, _])) =>
          fiberA.cancel.map(_ => f)

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
