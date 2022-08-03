package caio.std

import caio.Caio
import cats.CommutativeApplicative

class CaioApplicative[C, L] extends CommutativeApplicative[Caio[C, L, *]] {
  def pure[A](x: A): Caio[C, L, A] =
    Caio.pure(x)

  def ap[A, B](ff: Caio[C, L, A => B])(fa: Caio[C, L, A]): Caio[C, L, B] =
    fa.flatMap(a => ff.map(f => f(a)))
}
