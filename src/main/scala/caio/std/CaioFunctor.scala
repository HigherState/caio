package caio.std

import caio.Caio
import cats.Functor

trait CaioFunctor[C, L] extends Functor[Caio[C, L, *]] {
  def map[A, B](fa: Caio[C, L, A])(f: A => B): Caio[C, L, B] =
    fa.map(f)
}
