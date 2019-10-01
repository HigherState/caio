package caio.std

import caio._
import cats.Monoid
import cats.effect.{Concurrent, ContextShift, Fiber, IO}

class CaioConcurrent[C, V, L:Monoid](implicit CS:ContextShift[IO]) extends CaioAsync[C, V, L] with Concurrent[Caio[C, V, L, *]] {
  def start[A](fa: Caio[C, V, L, A]): Caio[C, V, L, Fiber[Caio[C, V, L, *], A]] =
    CaioKleisli { c =>
      IOResult {
        val fiberIO: IO[Fiber[IO, PureResult[C, V, L, A]]] = fa.toIOResult(c).io.start
        fiberIO.map { fiber =>
          val cancel: IO[Unit] = fiber.cancel

          val join = fiber.join
          SuccessResult(Fiber(
            CaioKleisli(_ => IOResult(join)),
            liftIO(cancel)
          ), Store.empty)
        }
      }
    }

  def racePair[A, B](fa: Caio[C, V, L, A], fb: Caio[C, V, L, B]): Caio[C, V, L, Either[(A, Fiber[Caio[C, V, L, *], B]), (Fiber[Caio[C, V, L, *], A], B)]] = ???
}
