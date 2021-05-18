package caio.mtl

import cats.effect.ConcurrentEffect

trait Effectful[F[_]] {

  def concurrentEffect: F[ConcurrentEffect[F]]
}

object Effectful {
  def apply[F[_], V](implicit E: Effectful[F]): Effectful[F] =
    E

  def concurrentEffect[F[_]](implicit E: Effectful[F]): F[ConcurrentEffect[F]] =
    E.concurrentEffect
}
