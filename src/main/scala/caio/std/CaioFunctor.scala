package caio.std

import caio.Caio
import cats.Functor

trait CaioFunctor[C, L] extends Functor[Caio[C, L, _]] {
  def map[A, B](fa: Caio[C, L, A])(f: A => B): Caio[C, L, B] =
    fa.map(f)
}

object CaioFunctor {
  def apply[C, L]: CaioFunctor[C, L] =
    new CaioFunctor[C, L] {}
}
