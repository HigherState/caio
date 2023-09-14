package caio.std

import caio.Caio
import cats.CommutativeApplicative

trait CaioApplicative[C, L] extends CommutativeApplicative[Caio[C, L, _]] {
  def pure[A](x: A): Caio[C, L, A] =
    Caio.pure(x)

  def ap[A, B](ff: Caio[C, L, A => B])(fa: Caio[C, L, A]): Caio[C, L, B] =
    fa.flatMap(a => ff.map(f => f(a)))
}

object CaioApplicative {
  def apply[C, L]: CaioApplicative[C, L] =
    new CaioApplicative[C, L] {}
}
