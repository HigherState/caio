package caio.std

import caio._
import cats.effect.{Bracket, ExitCase}

class CaioBracket[C, V, L] extends CaioMonadError[C, V, L] with Bracket[Caio[C, V, L, *], Throwable]  {

  def bracketCase[A, B](acquire: Caio[C, V, L, A])(use: A => Caio[C, V, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, V, L, Unit]): Caio[C, V, L, B] =
    BindCaio[C, V, L, A, B](acquire, a => {
      val useAcquired = use(a)
      val orHandleWithError =
        HandleErrorCaio[C, V, L, B](useAcquired,
          throwable => BindCaio[C, V, L, Unit, B](release(a, ExitCase.error(throwable)), _ => ErrorCaio(throwable)))
      val orHandleWithFailure =
        HandleFailureCaio[C, V, L, B](orHandleWithError,
          failures => BindCaio[C, V, L, Unit, B](
            release(a, ExitCase.error(CaioFailuresAsThrowable(failures))),
            _ => FailureCaio(failures.head, failures.tail)))
      BindCaio[C, V, L, B, B](orHandleWithFailure, b => MapCaio[C, V, L, Unit, B](release(a, ExitCase.Completed), _ => b))
    })
}
