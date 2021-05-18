package caio.std

import caio.Caio
import cats.Functor
import cats.mtl.Tell

class CaioTell[C, V, L] extends Tell[Caio[C, V, L, *], L] {
  override def functor: Functor[Caio[C, V, L, *]] =
    new CaioFunctor[C, V, L] {}

  def tell(l: L): Caio[C, V, L, Unit]             =
    Caio.tell(l)

  override def writer[A](a: A, l: L): Caio[C, V, L, A] =
    Caio.tell(l).as(a)

  override def tuple[A](ta: (L, A)): Caio[C, V, L, A] =
    writer(ta._2, ta._1)
}
