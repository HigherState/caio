package caio.std

import caio.Caio
import cats.Monoid
import cats.mtl.FunctorListen

class CaioFunctorListen[C, V, L:Monoid] extends CaioFunctorTell[C, V, L] with FunctorListen[Caio[C, V, L, *], L]  {

  def listen[A](fa: Caio[C, V, L, A]): Caio[C, V, L, (A, L)] =
    fa.mapWithStore[(A, L)]((a, s) => (a -> s.l, s))

  def listens[A, B](fa: Caio[C, V, L, A])(f: L => B): Caio[C, V, L, (A, B)] =
    fa.mapWithStore[(A, B)]((a, s) => (a -> f(s.l), s))

}
