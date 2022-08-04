package caio.std

import caio._
import cats.effect._
import cats.Monoid

class CaioEffect[C, L: Monoid](c: C)(onSuccess: (C, L) => IO[Unit] = (_: C, _: L) => IO.unit)(
  onError: (Throwable, C, L) => IO[Unit] = (_: Throwable, _: C, _: L) => IO.unit
) extends CaioAsync[C, L]
    with Effect[Caio[C, L, *]] {

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
        onError(ex, c, Monoid[L].empty)
          .flatMap(_ => cb(Left(ex)))
      case Right(FoldCaioSuccess(c2, l, a)) =>
        onSuccess(c2, l)
          .flatMap(_ => cb(Right(a)))
      case Right(FoldCaioError(c2, l, ex))  =>
        onError(ex, c2, l)
          .flatMap(_ => cb(Left(ex)))
    }

  /**
   * AsyncF application will discard any failure, Log or context change information.
   *
   * @param k
   * @tparam A
   * @return
   */
  override def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, L, Unit]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).flatMap(eval))
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, L, A](c, Monoid[L].empty, a))
    }

  def runAsync[A](fa: Caio[C, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    Caio
      .foldIO(fa, c)
      .runAsync(handle(_, cb))
}
