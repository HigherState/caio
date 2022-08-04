package caio.std

import caio.Caio
import cats.mtl.Censor
import cats.{CommutativeApplicative, Monoid}

class CaioCensor[C, L: Monoid] extends CaioListen[C, L] with Censor[Caio[C, L, *], L] {

  val applicative: CommutativeApplicative[Caio[C, L, *]] =
    new CaioApplicative[C, L]

  val monoid: Monoid[L] =
    implicitly[Monoid[L]]

  def censor[A](fa: Caio[C, L, A])(f: L => L): Caio[C, L, A] =
    fa.censor(f)

  override def clear[A](fa: Caio[C, L, A]): Caio[C, L, A] =
    fa.clear
}
