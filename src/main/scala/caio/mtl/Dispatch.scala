package caio.mtl

import cats.effect.Resource
import cats.effect.std.Dispatcher

trait Dispatch[F[_]] {
  def dispatcher: Resource[F, Dispatcher[F]]
}

object Dispatch {
  def apply[F[_]](implicit D: Dispatch[F]): Dispatch[F] =
    D

  def dispatcher[F[_]](implicit D: Dispatch[F]): Resource[F, Dispatcher[F]] =
    D.dispatcher
}
