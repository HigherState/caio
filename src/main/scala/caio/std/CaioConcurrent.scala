package caio.std

import caio._
import cats.effect.{Concurrent, ContextShift, Fiber, IO}

abstract class CaioConcurrent(implicit CS:ContextShift[IO]) extends Concurrent[Caio] with CaioAsync  {
  def start[A](fa: Caio[A]): Caio[Fiber[Caio, A]] =
    CaioKleisli { c =>
      IOResult[Fiber[Caio, A]] {
        val fiberIO: IO[Fiber[IO, PureResult[A]]] = fa.toIOResult(c).io.start
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

  def racePair[A, B](fa: Caio[A], fb: Caio[B]): Caio[Either[(A, Fiber[Caio, B]), (Fiber[Caio, A], B)]] = ???
}
