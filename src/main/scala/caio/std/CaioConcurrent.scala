package caio.std

import caio._
import cats.Monoid
import cats.effect._
import cats.effect.concurrent.Deferred

class CaioConcurrent[C, L: Monoid](implicit CS: ContextShift[IO]) extends CaioAsync[C, L] with Concurrent[Caio[C, L, *]] {

  def start[A](fa: Caio[C, L, A]): Caio[C, L, Fiber[Caio[C, L, *], A]] =
    KleisliCaio[C, L, Fiber[Caio[C, L, *], A]] { c =>
      Caio.foldIO(fa, c).start.map { fiber =>
        FoldCaioSuccess(c, Monoid[L].empty, fiber2Caio(fiber))
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

  override def continual[A, B](fa: Caio[C, L, A])(f: Either[Throwable,A] => Caio[C, L, B]): Caio[C, L, B] =
    Deferred.uncancelable[Caio[C, L, *], Either[Throwable, B]](this).flatMap { r =>
      fa.start[C, L, A].bracketCase[C, L, Fiber[Caio[C, L, *], A], A] { fiber =>
        Deferred.apply[Caio[C, L, *], A](this).flatMap { r2 =>
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

  private def fiber2Caio[A](fiber: Fiber[IO, FoldCaioPure[C, L, A]]): Fiber[Caio[C, L, *], A] = {
    val cancel: CancelToken[IO] = fiber.cancel
    val join                    = KleisliCaio[C, L, A](_ => fiber.join)
    Fiber(join, IOCaio(cancel))
  }
}
