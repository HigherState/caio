package caio.mtl

import caio.Caio
import cats.Functor

trait CaioFunctor extends Functor[Caio]{
  def map[A, B](fa: Caio[A])(f: A => B): Caio[B] =
    fa.map(f)
}
