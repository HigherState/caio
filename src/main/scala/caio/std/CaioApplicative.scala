package caio.std

import caio.Caio
import cats.CommutativeApplicative

class CaioApplicative[C, V, L] extends CommutativeApplicative[Caio[C, V, L, *]] {
  def pure[A](x: A): Caio[C, V, L, A] =
    Caio.pure(x)

  def ap[A, B](ff: Caio[C, V, L, A => B])(fa: Caio[C, V, L, A]): Caio[C, V, L, B] =
    fa.flatMap(a => ff.map(f => f(a)))
}
