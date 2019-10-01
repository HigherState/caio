package caio.std

import caio.{Caio, CaioState, Store}
import cats.{Applicative, Monoid}

class CaioApplicative[C, V, L:Monoid] extends Applicative[Caio[C, V, L, *]]{
  def pure[A](x: A): Caio[C, V, L, A] =
    CaioState[C, V, L, A](x, Store.empty[C, L])

  override def ap[A, B](ff: Caio[C, V, L, A => B])(fa: Caio[C, V, L, A]): Caio[C, V, L, B] =
    fa.flatMap(a => ff.map(_(a)))
}
