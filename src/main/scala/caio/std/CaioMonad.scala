package caio.std

import caio.Caio
import cats.StackSafeMonad

trait CaioMonad extends StackSafeMonad[Caio] with CaioApplicative {
  def flatMap[A, B](fa: Caio[A])(f: A => Caio[B]): Caio[B] =
    fa.flatMap(f)
}
