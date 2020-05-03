package caio.std

import caio.{Caio, ListenCaio, MapCaio}
import cats.Monoid
import cats.mtl.FunctorListen

class CaioFunctorListen[C, V, L:Monoid] extends CaioFunctorTell[C, V, L] with FunctorListen[Caio[C, V, L, *], L]  {

  def listen[A](fa: Caio[C, V, L, A]): Caio[C, V, L, (A, L)] =
    ListenCaio(fa)

  def listens[A, B](fa: Caio[C, V, L, A])(f: L => B): Caio[C, V, L, (A, B)] =
    MapCaio[C, V, L, (A, L), (A, B)](listen(fa), p => p._1 -> f(p._2))

}
