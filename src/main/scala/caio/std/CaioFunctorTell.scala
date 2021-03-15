package caio.std

import caio.Caio
import cats.Functor
import cats.mtl.FunctorTell

class CaioFunctorTell[C, V, L] extends FunctorTell[Caio[C, V, L, *], L]{
  override lazy val functor: Functor[Caio[C, V, L, *]] =
    new CaioFunctor[C, V, L] {}

  def tell(l: L): Caio[C, V, L, Unit] =
    Caio.tell(l)

  def writer[A](a: A, l: L): Caio[C, V, L, A] =
    Caio.tell(l).as(a)

  def tuple[A](ta: (L, A)): Caio[C, V, L, A] =
    writer(ta._2, ta._1)
}
