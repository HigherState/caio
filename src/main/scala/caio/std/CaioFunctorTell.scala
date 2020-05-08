package caio.std

import caio.{Caio, MapCaio, TellCaio}
import cats.{Functor, Monoid}
import cats.mtl.FunctorTell

class CaioFunctorTell[C, V, L:Monoid] extends FunctorTell[Caio[C, V, L, *], L]{
  override lazy val functor: Functor[Caio[C, V, L, *]] =
    new CaioFunctor[C, V, L] {}

  def tell(l: L): Caio[C, V, L, Unit] =
    TellCaio(l)

  def writer[A](a: A, l: L): Caio[C, V, L, A] =
    MapCaio[C, V, L, Unit, A](TellCaio(l), _ => a)

  def tuple[A](ta: (L, A)): Caio[C, V, L, A] =
    writer(ta._2, ta._1)
}
