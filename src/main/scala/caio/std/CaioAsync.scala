package caio.std

import caio.Caio
import cats.~>
import cats.effect.{Async, IO, Ref, Sync}
import cats.effect.kernel.{Cont, MonadCancel}

import scala.concurrent.ExecutionContext

class CaioAsync[C, L] extends CaioTemporal[C, L] with Async[Caio[C, L, *]] {
  final def suspend[A](hint: Sync.Type)(thunk: => A): Caio[C, L, A] =
    Caio.liftIO(IO.suspend(hint)(thunk))

  final def evalOn[A](fa: Caio[C, L, A], ec: ExecutionContext): Caio[C, L, A] =
    Caio.KleisliCaio[C, L, A] { c =>
      Async[IO].evalOn(Caio.foldIO(fa, c), ec)
    }

  final def executionContext: Caio[C, L, ExecutionContext] =
    Caio.liftIO(Async[IO].executionContext)

  final def cont[K, R](body: Cont[Caio[C, L, *], K, R]): Caio[C, L, R] =
    Caio.getContext[C].flatMap { c =>
      Caio
        .liftIO[(Either[Throwable, R], C)] {
          Async[IO].cont[K, (Either[Throwable, R], C)] {
            new Cont[IO, K, (Either[Throwable, R], C)] {
              import cats.syntax.all._

              def apply[G[_]: MonadCancel[*[_], Throwable]]
                : (Either[Throwable, K] => Unit, G[K], IO ~> G) => G[(Either[Throwable, R], C)] =
                (resume, get, lift) =>
                  for {
                    state  <- lift(Ref.of[IO, C](c))
                    newCont = new (Caio[C, L, *] ~> G) {
                                def apply[A](fa: Caio[C, L, A]): G[A] =
                                  for {
                                    foldPureCaio <- lift(Caio.foldIO[C, L, A](fa, c))
                                    _            <- lift(state.set(foldPureCaio.c))
                                    value        <- lift(foldPureCaio.toIO)
                                  } yield value
                              }
                    either <- body[G].apply(resume, get, newCont).attempt
                    newC   <- lift(state.get)
                  } yield (either, newC)
            }
          }
        }
        .flatMap { case (either, newC) =>
          Caio.setContext(newC) *> Caio.fromEither(either)
        }
    }

  final override def async[A](
    k: (Either[Throwable, A] => Unit) => Caio[C, L, Option[Caio[C, L, Unit]]]
  ): Caio[C, L, A] =
    super.async(args => Caio.defer(k(args)))
}

object CaioAsync {
  def apply[C, L]: CaioAsync[C, L] =
    new CaioAsync[C, L]
}
