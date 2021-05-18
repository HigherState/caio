package caio.std

import caio._
import cats.Monoid
import cats.effect.{Bracket, ExitCase}

class CaioBracket[C, V, L](implicit M: Monoid[L]) extends CaioMonadError[C, V, L] with Bracket[Caio[C, V, L, *], Throwable] {
  def bracketCase[A, B](acquire: Caio[C, V, L, A])(use: A => Caio[C, V, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, V, L, Unit]): Caio[C, V, L, B] =
    acquire.bracketCase(use)(release)
}