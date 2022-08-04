package caio.std

import caio._
import cats.Monoid
import cats.effect._

class CaioConcurrentEffect[C, L: Monoid](c: C)(onSuccess: (C, L) => IO[Unit] = (_: C, _: L) => IO.unit)(
  onError: (Throwable, C, L) => IO[Unit] = (_: Throwable, _: C, _: L) => IO.unit
)(implicit CS: ContextShift[IO])
    extends CaioConcurrent[C, L]
    with ConcurrentEffect[Caio[C, L, *]] {

  import cats.instances.vector._
  import cats.syntax.parallel._

  private def eval[A](value: FoldCaioPure[C, L, A]): IO[Unit] =
    value match {
      case FoldCaioSuccess(c1, l, _) =>
        onSuccess(c1, l)
      case FoldCaioError(c1, l, e)   =>
        onError(e, c1, l)
    }

  private def handle[A](either: Either[Throwable, FoldCaioPure[C, L, A]], cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
    either match {
      case Left(ex)                         =>
        Vector(cb(Left(ex)), onError(ex, c, Monoid[L].empty)).parSequence_
      case Right(FoldCaioSuccess(c2, l, a)) =>
        Vector(cb(Right(a)), onSuccess(c2, l)).parSequence_
      case Right(FoldCaioError(c2, l, ex))  =>
        Vector(cb(Left(ex)), onError(ex, c2, l)).parSequence_
    }

  override def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, L, Unit]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).flatMap(eval))
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, L, A](c, Monoid[L].empty, a))
    }

  def runCancelable[A](fa: Caio[C, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[Caio[C, L, *]]] =
    Caio
      .foldIO(fa, c)
      .runCancelable(handle(_, cb))
      .map(Caio.liftIO)

  def runAsync[A](fa: Caio[C, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    Caio
      .foldIO(fa, c)
      .runAsync(handle(_, cb))
}
