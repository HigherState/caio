package caio.mtl

import cats.arrow.FunctionK
import cats.effect.{CancelToken, Concurrent, ConcurrentEffect, ExitCase, Fiber, IO, SyncIO}

class ConcurrentEffectFunctionK[F[_], G[_]](
  concurrentEffect:ConcurrentEffect[G],
  backwards:FunctionK[F, G],
  forwards:FunctionK[G, F])(implicit C:Concurrent[F]) extends ConcurrentEffect[F]{
  def runCancelable[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[F]] =
    concurrentEffect.runCancelable(backwards(fa))(cb).map{forwards.apply}

  def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
    concurrentEffect.runAsync(backwards(fa))(cb)

  def start[A](fa: F[A]): F[Fiber[F, A]] =
    C.start(fa)

  def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]] =
    C.racePair(fa, fb)

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
    C.async(k)

  def asyncF[A](k: (Either[Throwable, A] => Unit) => F[Unit]): F[A] =
    C.asyncF(k)

  def suspend[A](thunk: => F[A]): F[A] =
    C.suspend(thunk)

  def bracketCase[A, B](acquire: F[A])(use: A => F[B])(release: (A, ExitCase[Throwable]) => F[Unit]): F[B] =
    C.bracketCase(acquire)(use)(release)

  def pure[A](x: A): F[A] =
    C.pure(x)

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    C.flatMap(fa)(f)

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    C.tailRecM(a)(f)

  def raiseError[A](e: Throwable): F[A] =
    C.raiseError(e)

  def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
    C.handleErrorWith(fa)(f)
}
