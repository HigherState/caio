package caio

import cats. StackSafeMonad

trait CaioMonad extends StackSafeMonad[Caio]{
  def flatMap[A, B](fa: Caio[A])(f: A => Caio[B]): Caio[B] =
    fa.flatMap(f)

  def pure[A](x: A): Caio[A] =
    CaioPure(x)
}
