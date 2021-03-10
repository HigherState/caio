package caio.std

import caio.Caio
import cats.mtl.FunctorListen

class CaioFunctorListen[C, V, L] extends CaioFunctorTell[C, V, L] with FunctorListen[Caio[C, V, L, *], L]  {
  def listen[A](fa: Caio[C, V, L, A]): Caio[C, V, L, (A, L)] =
    fa.listen

  def listens[A, B](fa: Caio[C, V, L, A])(f: L => B): Caio[C, V, L, (A, B)] =
    fa.listen.map { case (a, l) => a -> f(l) }
}
