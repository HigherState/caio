package caio.std

import caio.{BindCaio, Caio, MapCaio, PureCaio}
import cats.{Applicative, Monoid}

class CaioApplicative[C, V, L:Monoid] extends Applicative[Caio[C, V, L, *]]{
  def pure[A](x: A): Caio[C, V, L, A] =
    PureCaio(x)

  def ap[A, B](ff: Caio[C, V, L, A => B])(fa: Caio[C, V, L, A]): Caio[C, V, L, B] =
    BindCaio[C, V, L, A, B](fa, a => MapCaio[C, V, L, A => B, B](ff, f => f(a)))
}
