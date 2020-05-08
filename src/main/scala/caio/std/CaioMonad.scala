package caio.std

import caio.{BindCaio, Caio}
import cats.StackSafeMonad

class CaioMonad[C, V, L] extends CaioApplicative[C, V, L] with StackSafeMonad[Caio[C, V, L, *]] {
  def flatMap[A, B](fa: Caio[C, V, L, A])(f: A => Caio[C, V, L, B]): Caio[C, V, L, B] =
    BindCaio(fa, f)
}
