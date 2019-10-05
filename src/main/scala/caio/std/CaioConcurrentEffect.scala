package caio.std

import caio._
import cats.Monoid
import cats.effect._

class CaioConcurrentEffect[C, V, L:Monoid]
  (c:C)
  (onSuccess:(C, L) => IO[Unit])
  (onFailure:((ErrorOrFailure[V], C, L) => IO[Unit]))
  (implicit CS:ContextShift[IO]) extends CaioConcurrent[C, V, L] with ConcurrentEffect[Caio[C, V, L, *]] {

  def runCancelable[A](fa: Caio[C, V, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[Caio[C, V, L, *]]] = {
    val cancelToken =
      fa.eval(c).runCancelable {
        case Left(e) =>
          cb(Left(e))
            .flatMap(_ => onFailure(Left(e), c, implicitly[Monoid[L]].empty))
        case Right((Left(e), c, l)) =>
          cb(Left(e.toThrowable))
            .flatMap(_ => onFailure(e, c, l))
        case Right((Right(a), c, l)) =>
          cb(Right(a))
            .flatMap(_ => onSuccess(c, l))
      }
    cancelToken.map(liftIO)
  }

  def runAsync[A](fa: Caio[C, V, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    fa.eval(c).flatMap{
      case (Right(a), c, l) =>
        onSuccess(c, l).map(_ => a)
      case (Left(f), c, l) =>
        onFailure(f, c, l).flatMap{_ =>
          IO.raiseError(f.toThrowable)
        }
    }.runAsync(_ => IO.unit)
}
