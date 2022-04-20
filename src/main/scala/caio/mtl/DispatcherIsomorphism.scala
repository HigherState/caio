package caio.mtl

import caio.<~>
import cats.effect.std.Dispatcher

import scala.concurrent.Future

class DispatcherIsomorphism[F[_], G[_]](dispatcher: Dispatcher[G], isomorphism: F <~> G) extends Dispatcher[F] {
  override def unsafeToFutureCancelable[A](fa: F[A]): (Future[A], () => Future[Unit]) =
    dispatcher.unsafeToFutureCancelable(isomorphism.apply(fa))
}
