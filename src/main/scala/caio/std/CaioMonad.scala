package caio.std

import caio.Caio
import cats.{CommutativeApplicative, CommutativeMonad, Functor, StackSafeMonad}

trait CaioApplicative[C, L] extends CommutativeApplicative[Caio[C, L, *]] {
  def pure[A](x: A): Caio[C,  L, A] =
    Caio.pure(x)

  def ap[A, B](ff: Caio[C, L, A => B])(fa: Caio[C, L, A]): Caio[C, L, B] =
    fa.flatMap(a => ff.map(f => f(a)))
}

trait CaioFunctor[C, L] extends Functor[Caio[C, L, *]] {
  def map[A, B](fa: Caio[C, L, A])(f: A => B): Caio[C, L, B] =
    fa.map(f)
}

trait CaioMonad[C, L]
    extends CaioApplicative[C, L]
    with StackSafeMonad[Caio[C, L, *]]
    with CommutativeMonad[Caio[C, L, *]] {
  def flatMap[A, B](fa: Caio[C, L, A])(f: A => Caio[C, L, B]): Caio[C, L, B] =
    fa.flatMap(f)

  override def map[A, B](fa: Caio[C, L, A])(f: A => B): Caio[C, L, B] =
    fa.map(f)
}
