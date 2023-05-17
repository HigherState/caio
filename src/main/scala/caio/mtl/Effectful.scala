package caio.mtl

import cats.effect.std.Dispatcher

trait Effectful[F[_]] {
  def dispatcher: F[Dispatcher[F]]
}

object Effectful {
  def apply[F[_], V](implicit E: Effectful[F]): Effectful[F] =
    E

  def dispatcher[F[_]](implicit E: Effectful[F]): F[Dispatcher[F]] =
    E.dispatcher
}
