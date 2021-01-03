package caio.mtl

import caio.<~>
import cats.effect._

class ConcurrentEffectIsomorphism[F[_], G[_]](
  concurrentEffect:ConcurrentEffect[G],
  isomorphism: F <~> G) extends ConcurrentEffect[F]{

  private val invert = isomorphism.invert

  def runCancelable[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[F]] =
    concurrentEffect.runCancelable(isomorphism(fa))(cb)
      .map(isomorphism.unapply)

  def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    concurrentEffect.runAsync(isomorphism(fa))(cb)

  def start[A](fa: F[A]): F[Fiber[F, A]] =
    invert {
      concurrentEffect.map(concurrentEffect.start(isomorphism(fa))){_.mapK[F](invert)}
    }

  def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]] =
    isomorphism.unapply {
      concurrentEffect.map(concurrentEffect.racePair(isomorphism(fa), isomorphism(fb))){
        case Left((a, fb)) => Left(a -> fb.mapK(invert))
        case Right((fa, b)) => Right(fa.mapK(invert) -> b)
      }
    }

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
    invert(concurrentEffect.async(k))

  def asyncF[A](k: (Either[Throwable, A] => Unit) => F[Unit]): F[A] =
    invert(concurrentEffect.asyncF(k.andThen(isomorphism.apply)))

  def suspend[A](thunk: => F[A]): F[A] =
    invert(concurrentEffect.suspend(isomorphism(thunk)))

  def bracketCase[A, B](acquire: F[A])(use: A => F[B])(release: (A, ExitCase[Throwable]) => F[Unit]): F[B] = {
    def releaseG:(A, ExitCase[Throwable]) => G[Unit] = (a, e) => isomorphism(release(a, e))
    invert(concurrentEffect.bracketCase(isomorphism(acquire))(use.andThen(isomorphism.apply))(releaseG))
  }

  def pure[A](x: A): F[A] =
    invert(concurrentEffect.pure(x))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    invert(concurrentEffect.flatMap(isomorphism(fa))(f.andThen(isomorphism.apply)))

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    invert(concurrentEffect.tailRecM(a)(f.andThen(isomorphism.apply)))

  def raiseError[A](e: Throwable): F[A] =
    invert(concurrentEffect.raiseError(e))

  def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
    invert(concurrentEffect.handleErrorWith(isomorphism(fa))(f.andThen(isomorphism.apply)))
}
