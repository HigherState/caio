package caio.std

import caio.Caio
import cats.mtl.ApplicativeCensor
import cats.{CommutativeApplicative, Monoid}

class CaioApplicativeCensor[C, V, L: Monoid] extends CaioFunctorListen[C, V, L] with ApplicativeCensor[Caio[C, V, L, *], L] {

  val applicative: CommutativeApplicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  val monoid: Monoid[L] =
    implicitly[Monoid[L]]

  def censor[A](fa: Caio[C, V, L, A])(f: L => L): Caio[C, V, L, A] =
    fa.censor(f)

  def clear[A](fa: Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.clear
}
