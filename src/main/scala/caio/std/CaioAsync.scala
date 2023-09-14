package caio.std

import caio.Caio
import cats.{Monoid, ~>}
import cats.effect.{Async, IO, MonadCancelThrow, Ref, Sync}
import cats.effect.kernel.Cont

import scala.concurrent.ExecutionContext

class CaioAsync[C, L] extends CaioTemporal[C, L] with Async[Caio[C, L, _]] {
  final def suspend[A](hint: Sync.Type)(thunk: => A): Caio[C, L, A] =
    Caio.liftIO(IO.suspend(hint)(thunk))

  final def evalOn[A](fa: Caio[C, L, A], ec: ExecutionContext): Caio[C, L, A] =
    Caio.KleisliCaio[C, L, A] { c =>
      Async[IO].evalOn(Caio.foldIO(fa, c), ec)
    }

  final def executionContext: Caio[C, L, ExecutionContext] =
    Caio.liftIO(IO.executionContext)

  final def cont[K, R](body: Cont[Caio[C, L, _], K, R]): Caio[C, L, R] =
    Caio.getContext[C].flatMap { c =>
      Caio
        .liftIO[(Either[Throwable, R], C, Option[(L, Monoid[L])])] {
          Async[IO].cont[K, (Either[Throwable, R], C, Option[(L, Monoid[L])])] {
            new Cont[IO, K, (Either[Throwable, R], C, Option[(L, Monoid[L])])] {
              import cats.syntax.all._

              def apply[G[_]: MonadCancelThrow]
                : (Either[Throwable, K] => Unit, G[K], IO ~> G) => G[(Either[Throwable, R], C, Option[(L, Monoid[L])])] =
                (resume, get, lift) =>
                  for {
                    state  <- lift(Ref.of[IO, C](c))
                    logs   <- lift(Ref.of[IO, Option[(L, Monoid[L])]](None))
                    newCont = new (Caio[C, L, _] ~> G) {
                                def apply[A](fa: Caio[C, L, A]): G[A] =
                                  for {
                                    foldPureCaio <- lift(Caio.foldIO[C, L, A](fa, c))
                                    _            <- lift(state.set(foldPureCaio.c))
                                    _            <- lift(logs.set(foldPureCaio.opt))
                                    value        <- lift(foldPureCaio.toIO)
                                  } yield value
                              }
                    either <- body[G].apply(resume, get, newCont).attempt
                    newC   <- lift(state.get)
                    newL   <- lift(logs.get)
                  } yield (either, newC, newL)
            }
          }
        }
        .flatMap { case (either, newC, newL) =>
          Caio.setContext(newC) *>
            newL.fold[Caio[C, L, Unit]](Caio.unit) { case (l, m) => Caio.tell(l)(m) } *>
            Caio.fromEither(either)
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
