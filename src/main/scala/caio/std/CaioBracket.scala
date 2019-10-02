package caio.std

import caio.{Caio, CaioError, CaioKleisli, Store}
import cats.Monoid
import cats.effect.{Bracket, ExitCase}

class CaioBracket[C, V, L: Monoid] extends CaioMonadError[C, V, L] with Bracket[Caio[C, V, L, *], Throwable]  {

  def bracketCase[A, B](acquire: Caio[C, V, L, A])(use: A => Caio[C, V, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, V, L, Unit]): Caio[C, V, L, B] =
    this.flatMap(acquire) { a =>
      val ua = use(a)
      CaioKleisli { c =>
        ua
          .handleError{ ex =>
            release(a, ExitCase.error(ex))
              .flatMap{ _ => CaioError(Left(ex), Store.empty[C, L])}
          }
          .handleFailures{v =>
            release(a, ExitCase.error(CaioFailuresAsThrowable(v)))
              .flatMap{ _ => CaioError(Right(v), Store.empty[C, L])}
          }
          .flatMap { b =>
            release(a, ExitCase.Completed)
              .map(_ => b)
          }.toResult(c)
      }
    }
}
