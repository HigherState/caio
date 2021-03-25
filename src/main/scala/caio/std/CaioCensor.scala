package caio.std

import caio.Caio
import cats.mtl.Censor
import cats.{CommutativeApplicative, Monoid}

class CaioCensor[C, V, L: Monoid] extends CaioListen[C, V, L] with Censor[Caio[C, V, L, *], L] {

  val applicative: CommutativeApplicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  val monoid: Monoid[L] =
    implicitly[Monoid[L]]

  def censor[A](fa: Caio[C, V, L, A])(f: L => L): Caio[C, V, L, A] =
    fa.censor(f)

  override def clear[A](fa: Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa.clear
}
