package caio.std

import caio._
import cats.effect._

trait CaioConcurrent[C, L] extends CaioSpawn[C, L] with Concurrent[Caio[C, L, *]] {
  final def ref[A](a: A): Caio[C, L, Ref[Caio[C, L, *], A]] =
    Caio.ref[C, L, A](a)

  final def deferred[A]: Caio[C, L, Deferred[Caio[C, L, *], A]] =
    Caio.deferred[C, L, A]
}

object CaioConcurrent {
  def apply[C, L]: CaioConcurrent[C, L] =
    new CaioConcurrent[C, L] with CaioUnique[C, L] {
      def never[A]: Caio[C, L, A] =
        Caio.never[A]
    }
}
