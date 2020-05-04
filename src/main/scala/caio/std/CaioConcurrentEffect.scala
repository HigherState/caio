package caio.std

import caio._
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect._

class CaioConcurrentEffect[C, V, L:Monoid]
  (c:C)
  (onSuccess:(C, L) => IO[Unit])
  (onError:((Throwable, C, L) => IO[Unit]))
  (onFailure:((NonEmptyList[V], C, L) => IO[Unit]))
  (implicit CS:ContextShift[IO]) extends CaioConcurrent[C, V, L] with ConcurrentEffect[Caio[C, V, L, *]] {


  private def eval[A](value:FoldCaioPure[C, V, L, A]): IO[Unit] =
    value match {
      case FoldCaioSuccess(c1, l, _) =>
        onSuccess(c1, l)
      case FoldCaioError(c1, l, e) =>
        onError(e, c1, l)
      case FoldCaioFailure(c1, l, head, tail) =>
        onFailure(NonEmptyList(head, tail), c1, l)
    }

  private def handle[A](either:Either[Throwable, A], onSuccess:A => IO[Unit]):IO[Unit] =
    either match {
      case Left(CaioFailuresAsThrowable(NonEmptyList(head:V, tail:List[V]))) =>
        onFailure(NonEmptyList(head, tail), c, Monoid[L].empty)
      case Left(e) =>
        onError(e, c, Monoid[L].empty)
      case Right(a) =>
        onSuccess(a)
    }
  /**
   * AsyncF application will discard any failure, Log or context change information.
   *
   * @param k
   * @tparam A
   * @return
   */
  override def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
    KleisliCaio[C, V, L, A]{ c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).flatMap(eval))
      FoldCaioIO(IO.asyncF(k2).map(a => FoldCaioSuccess[C, V, L, A](c, Monoid[L].empty, a)))
    }

  def runCancelable[A](fa: Caio[C, V, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[Caio[C, V, L, *]]] =
    Caio
      .foldIO(fa, c)
      .runCancelable(e => handle[FoldCaioPure[C, V, L, A]](e, eval))
      .map(IOCaio(_))

  def runAsync[A](fa: Caio[C, V, L, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    Caio
      .foldIO(fa, c).flatMap(eval)
      .runAsync(e => handle(e, _ => IO.unit))
}
