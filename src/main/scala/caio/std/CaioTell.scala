package caio.std

import caio.Caio
import cats.{Functor, Monoid}
import cats.mtl.Tell

trait CaioTell[C, L] extends Tell[Caio[C, L, _], L] {
  val monoid: Monoid[L]

  override def functor: Functor[Caio[C, L, _]] =
    CaioFunctor[C, L]

  def tell(l: L): Caio[C, L, Unit] =
    Caio.tell(l)(monoid)

  override def writer[A](a: A, l: L): Caio[C, L, A] =
    Caio.tell(l)(monoid).as(a)

  override def tuple[A](ta: (L, A)): Caio[C, L, A] =
    writer(ta._2, ta._1)
}

object CaioTell {
  def apply[C, L](implicit M: Monoid[L]): CaioTell[C, L] =
    new CaioTell[C, L] { val monoid: Monoid[L] = M }
}
