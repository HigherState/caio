package caio.std

import caio._
import cats.Monoid
import cats.effect._
import cats.effect.concurrent.Deferred

class CaioConcurrent[C, V, L: Monoid](implicit CS: ContextShift[IO])
    extends CaioAsync[C, V, L]
    with Concurrent[Caio[C, V, L, *]] {

  def start[A](fa: Caio[C, V, L, A]): Caio[C, V, L, Fiber[Caio[C, V, L, *], A]] =
    KleisliCaio[C, V, L, Fiber[Caio[C, V, L, *], A]] { c =>
      Caio.foldIO(fa, c).start.map { fiber =>
        FoldCaioSuccess(c, Monoid[L].empty, fiber2Caio(fiber))
      }
    }

  def racePair[A, B](
    fa: Caio[C, V, L, A],
    fb: Caio[C, V, L, B]
  ): Caio[C, V, L, Either[(A, Fiber[Caio[C, V, L, *], B]), (Fiber[Caio[C, V, L, *], A], B)]] =
    KleisliCaio[C, V, L, Either[(A, Fiber[Caio[C, V, L, *], B]), (Fiber[Caio[C, V, L, *], A], B)]] { c =>
      val sa = Caio.foldIO(fa, c)
      val sb = Caio.foldIO(fb, c)
      IO.racePair(sa, sb).flatMap {
        case Left((e: FoldCaioError[C, V, L, _], fiberB)) =>
          fiberB.cancel.map(_ => e)

        case Left((f: FoldCaioFailure[C, V, L, _], fiberB)) =>
          fiberB.cancel.map(_ => f)

        case Left((s: FoldCaioSuccess[C, V, L, A], fiberB)) =>
          IO(s.map(a => Left(a -> fiber2Caio(fiberB))))

        case Right((fiberA, e: FoldCaioError[C, V, L, _]))  =>
          fiberA.cancel.map(_ => e)

        case Right((fiberA, f: FoldCaioFailure[C, V, L, _])) =>
          fiberA.cancel.map(_ => f)

        case Right((fiberA, s: FoldCaioSuccess[C, V, L, B])) =>
          IO(s.map(b => Right(fiber2Caio(fiberA) -> b)))
      }
    }

  override def continual[A, B](fa: Caio[C, V, L, A])(f: Either[Throwable, A] => Caio[C, V, L, B]): Caio[C, V, L, B] =
    Deferred.uncancelable[Caio[C, V, L, *], Either[Throwable, B]](this).flatMap { r =>
      fa.start[C, V, L, A].bracketCase[C, V, L, Fiber[Caio[C, V, L, *], A], A] { fiber =>
        Deferred.apply[Caio[C, V, L, *], A](this).flatMap { r2 =>
          fiber.join.flatTap(r2.complete).guaranteeCase {
            case ExitCase.Completed => r2.get.flatMap(a => f(Right(a))).attempt.flatMap(r.complete)
            case ExitCase.Error(ex) => f(Left(ex)).attempt.flatMap(r.complete)
            case _                  => Caio.unit
          }
        }
      } {
        case (fiber, ExitCase.Canceled) => fiber.cancel
        case _ => Caio.unit
      }.attempt *> r.get.rethrow
    }

  private def fiber2Caio[A](fiber: Fiber[IO, FoldCaioPure[C, V, L, A]]): Fiber[Caio[C, V, L, *], A] = {
    val cancel: CancelToken[IO] = fiber.cancel
    val join                    = KleisliCaio[C, V, L, A](_ => fiber.join)
    Fiber(join, IOCaio(cancel))
  }
}
