package caio.std

import caio.Caio
import cats.{Monoid, StackSafeMonad}

class CaioMonad[C, V, L: Monoid] extends CaioApplicative[C, V, L] with StackSafeMonad[Caio[C, V, L, *]] {
  def flatMap[A, B](fa: Caio[C, V, L, A])(f: A => Caio[C, V, L, B]): Caio[C, V, L, B] =
    fa.flatMap(f)
}
