package caio.std

import caio._
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect._

class CaioConcurrentEffect[C, V, L: Monoid](c: C)(onSuccess: (C, L) => IO[Unit] = (_: C, _: L) => IO.unit)(
  onError: (Throwable, C, L) => IO[Unit] = (_: Throwable, _: C, _: L) => IO.unit
)(onFailure: (NonEmptyList[V], C, L) => IO[Unit] = (_: NonEmptyList[V], _: C, _: L) => IO.unit)(implicit
  CS: ContextShift[IO]
) extends CaioConcurrent[C, V, L]
    with ConcurrentEffect[Caio[C, V, L, *]] {

  import cats.instances.vector._
  import cats.syntax.parallel._

  private def eval[A](value: FoldCaioPure[C, V, L, A]): IO[Unit] =
    value match {
      case FoldCaioSuccess(c1, l, _)          =>
        onSuccess(c1, l)
      case FoldCaioError(c1, l, e)            =>
        onError(e, c1, l)
      case FoldCaioFailure(c1, l, head, tail) =>
        onFailure(NonEmptyList(head, tail), c1, l)
    }

  private def handle[A](
    either: Either[Throwable, FoldCaioPure[C, V, L, A]],
    cb: Either[Throwable, A] => IO[Unit]
  ): IO[Unit] =
    either match {
      case Left(ex)                                                                                        =>
        Vector(cb(Left(ex)), onError(ex, c, Monoid[L].empty)).parSequence_
      case Right(FoldCaioSuccess(c2, l, a))                                                                =>
        Vector(cb(Right(a)), onSuccess(c2, l)).parSequence_
      case Right(FoldCaioFailure(c2, l, head, tail))                                                       =>
        val nel = NonEmptyList(head, tail)
        Vector(cb(Left(CaioFailuresAsThrowable(nel))), onFailure(nel, c2, l)).parSequence_
      case Right(FoldCaioError(c2, l, ex @ CaioFailuresAsThrowable(failures: NonEmptyList[V @unchecked]))) =>
        Vector(cb(Left(ex)), onFailure(failures, c2, l)).parSequence_
      case Right(FoldCaioError(c2, l, ex))                                                                 =>
        Vector(cb(Left(ex)), onError(ex, c2, l)).parSequence_
    }

  override def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
    KleisliCaio[C, V, L, A] { c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).flatMap(eval))
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, V, L, A](c, Monoid[L].empty, a))
    }

  def runCancelable[A](
    fa: Caio[C, V, L, A]
  )(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[Caio[C, V, L, *]]] =
    Caio
      .foldIO(fa, c)
      .runCancelable(handle(_, cb))
      .map(Caio.liftIO)

  def runAsync[A](fa: Caio[C, V, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    Caio
      .foldIO(fa, c)
      .runAsync(handle(_, cb))
}
