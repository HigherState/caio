package caio.mtl

import caio.<~>
import cats.~>
import cats.effect._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext

class AsyncIsomorphism[F[_], G[_]](async: Async[G], isomorphism: F <~> G) extends Async[F] {

  private val invert = isomorphism.invert

  def pure[A](x: A): F[A] =
    invert(async.pure(x))

  def raiseError[A](e: Throwable): F[A] =
    invert(async.raiseError(e))

  def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
    invert(async.handleErrorWith(isomorphism(fa))(f.andThen(isomorphism.apply)))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    invert(async.flatMap(isomorphism(fa))(f.andThen(isomorphism.apply)))

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    invert(async.tailRecM(a)(f.andThen(isomorphism.apply)))

  def forceR[A, B](fa: F[A])(fb: F[B]): F[B] =
    invert(async.forceR(isomorphism(fa))(isomorphism(fb)))

  def uncancelable[A](body: Poll[F] => F[A]): F[A] =
    invert {
      async.uncancelable { poll =>
        isomorphism {
          body(new Poll[F] {
            def apply[X](fa: F[X]): F[X] =
              invert(poll(isomorphism(fa)))
          })
        }
      }
    }

  def canceled: F[Unit] =
    invert(async.canceled)

  def onCancel[A](fa: F[A], fin: F[Unit]): F[A] =
    invert(async.onCancel(isomorphism(fa), isomorphism(fin)))

  def monotonic: F[FiniteDuration] =
    invert(async.monotonic)

  def realTime: F[FiniteDuration] =
    invert(async.realTime)

  def suspend[A](hint: kernel.Sync.Type)(thunk: => A): F[A] =
    invert(async.suspend(hint)(thunk))

  def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] =
    invert {
      async.map(async.start(isomorphism(fa))) { fiber =>
        new Fiber[F, Throwable, A] {
          def cancel: F[Unit] =
            invert(fiber.cancel)

          def join: F[Outcome[F, Throwable, A]] =
            invert(async.map(fiber.join) {
              case Outcome.Canceled()    =>
                Outcome.canceled[F, Throwable, A]
              case Outcome.Errored(ex)   =>
                Outcome.errored[F, Throwable, A](ex)
              case Outcome.Succeeded(ga) =>
                Outcome.succeeded[F, Throwable, A](invert(ga))
            })
        }
      }
    }

  def cede: F[Unit] =
    invert(async.cede)

  def ref[A](a: A): F[Ref[F, A]] =
    invert {
      async.map(async.ref(a))(_.mapK[F](invert)(async))
    }

  def deferred[A]: F[Deferred[F, A]] =
    invert {
      async.map(async.deferred[A])(_.mapK[F](invert))
    }

  def sleep(time: FiniteDuration): F[Unit] =
    invert(async.sleep(time))

  def evalOn[A](fa: F[A], ec: ExecutionContext): F[A] =
    invert(async.evalOn(isomorphism(fa), ec))

  def executionContext: F[ExecutionContext] =
    invert(async.executionContext)

  def cont[K, R](body: Cont[F, K, R]): F[R] =
    invert {
      async.cont {
        new Cont[G, K, R] {
          def apply[GG[_]: MonadCancel[*[_], Throwable]]: (Either[Throwable, K] => Unit, GG[K], G ~> GG) => GG[R] = {
            (resume, get, lift) =>
              body[GG].apply(resume, get, isomorphism.andThen(lift))
          }
        }
      }
    }
}
