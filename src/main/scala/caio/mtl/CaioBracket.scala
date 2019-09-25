package caio.mtl

import caio.{Caio, CaioError, CaioKleisli, State}
import cats.effect.{Bracket, ExitCase}

trait CaioBracket extends Bracket[Caio, Throwable] with CaioMonadError {

  def bracketCase[A, B](acquire: Caio[A])(use: A => Caio[B])(release: (A, ExitCase[Throwable]) => Caio[Unit]): Caio[B] =
    this.flatMap(acquire) { a =>
      val ua = use(a)
      CaioKleisli { c =>
        ua.handleError{ ex =>
          release(a, ExitCase.error(ex))
            .flatMap{ _ => CaioError(ex, State.empty)}
          }
          .flatMap { b =>
            release(a, ExitCase.Completed)
              .map(_ => b)
          }.toResult(c)
      }
    }
}

