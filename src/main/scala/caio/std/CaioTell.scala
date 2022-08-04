package caio.std

import caio.Caio
import cats.Functor
import cats.mtl.Tell

class CaioTell[C, L] extends Tell[Caio[C, L, *], L] {
  override def functor: Functor[Caio[C, L, *]] =
    new CaioFunctor[C, L] {}

  def tell(l: L): Caio[C, L, Unit]             =
    Caio.tell(l)

  override def writer[A](a: A, l: L): Caio[C, L, A] =
    Caio.tell(l).as(a)

  override def tuple[A](ta: (L, A)): Caio[C, L, A] =
    writer(ta._2, ta._1)
}
