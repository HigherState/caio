package caio.std

import caio.{Caio, PureCaio, WithLogCaio}
import cats.Monoid
import cats.mtl.FunctorListen

class CaioFunctorListen[C, V, L:Monoid] extends CaioFunctorTell[C, V, L] with FunctorListen[Caio[C, V, L, *], L]  {

  def listen[A](fa: Caio[C, V, L, A]): Caio[C, V, L, (A, L)] =
    WithLogCaio(fa, (a:A, l:L) => l -> PureCaio(a -> l))

  def listens[A, B](fa: Caio[C, V, L, A])(f: L => B): Caio[C, V, L, (A, B)] =
    WithLogCaio(fa, (a:A, l:L) => l -> PureCaio(a -> f(l)))

}
