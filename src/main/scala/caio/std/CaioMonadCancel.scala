package caio.std

import caio.{Caio, Logger, OutcomeCaio}
import cats.effect.kernel.{CancelScope, Poll}
import cats.effect.{IO, MonadCancel}
import cats.effect.kernel.Outcome

trait CaioMonadCancel[C, L] extends CaioMonadError[C, L] with MonadCancel[Caio[C, L, _], Throwable] {
  def forceR[A, B](caioA: Caio[C, L, A])(caioB: Caio[C, L, B]): Caio[C, L, B] =
    caioA.forceR(caioB)

  def uncancelable[A](body: Poll[Caio[C, L, _]] => Caio[C, L, A]): Caio[C, L, A] =
    Caio.uncancelable(body)

  def canceled: Caio[C, L, Unit] =
    Caio.liftIO(MonadCancel[IO, Throwable].canceled)

  def onCancel[A](caio: Caio[C, L, A], finalizer: Caio[C, L, Unit]): Caio[C, L, A] =
    caio.onCancel(finalizer)

  override def guaranteeCase[A](caio: Caio[C, L, A])(finalizer: OutcomeCaio[C, L, A] => Caio[C, L, Unit]): Caio[C, L, A] =
    uncancelable { poll =>
      val finalized = onCancel(poll(caio), Caio.defer(finalizer(Outcome.canceled)))
      val handled   = onError(finalized) { case e =>
        handleError(Caio.defer(finalizer(Outcome.errored(e))))(Logger.reportFailure)
      }
      flatTap(handled)(a => Caio.defer(finalizer(Outcome.succeeded(pure(a)))))
    }
}

object CaioMonadCancel {
  def apply[C, L]: CaioMonadCancel[C, L] =
    new CaioMonadCancel[C, L] {
      final override def rootCancelScope: CancelScope =
        CancelScope.Cancelable
    }
}
