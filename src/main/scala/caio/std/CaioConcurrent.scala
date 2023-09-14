package caio.std

import caio._
import cats.effect._

trait CaioConcurrent[C, L] extends CaioSpawn[C, L] with Concurrent[Caio[C, L, _]] {
  final def ref[A](a: A): Caio[C, L, Ref[Caio[C, L, _], A]] =
    Caio.ref[C, L, A](a)

  final def deferred[A]: Caio[C, L, Deferred[Caio[C, L, _], A]] =
    Caio.deferred[C, L, A]

  override def racePair[A, B](fa: Caio[C, L, A], fb: Caio[C, L, B]): Caio[C, L,
    Either[
      (Outcome[Caio[C, L, _], Throwable, A], Fiber[Caio[C, L, _], Throwable, B]),
      (Fiber[Caio[C, L, _], Throwable, A], Outcome[Caio[C, L, _], Throwable, B])
    ]
  ] =
    super[CaioSpawn].racePair(fa, fb)
}

object CaioConcurrent {
  def apply[C, L]: CaioConcurrent[C, L] =
    new CaioConcurrent[C, L] with CaioUnique[C, L] {
      def never[A]: Caio[C, L, A] =
        Caio.never[A]
    }
}
