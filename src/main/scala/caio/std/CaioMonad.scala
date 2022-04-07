package caio.std

import caio.Caio
import cats.{CommutativeMonad, StackSafeMonad}

trait CaioMonad[C, L]
    extends CaioApplicative[C, L]
    with StackSafeMonad[Caio[C, L, *]]
    with CommutativeMonad[Caio[C, L, *]] {
  def flatMap[A, B](fa: Caio[C, L, A])(f: A => Caio[C, L, B]): Caio[C, L, B] =
    fa.flatMap(f)

  override def map[A, B](fa: Caio[C, L, A])(f: A => B): Caio[C, L, B] =
    fa.map(f)
}

object CaioMonad {
  def apply[C, L]: CaioMonad[C, L] =
    new CaioMonad[C, L] {}
}
