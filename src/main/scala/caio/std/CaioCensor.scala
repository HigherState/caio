package caio.std

import caio.Caio
import cats.{CommutativeApplicative, Monoid}
import cats.mtl.Censor

trait CaioCensor[C, L] extends CaioListen[C, L] with Censor[Caio[C, L, _], L] {
  def censor[A](fa: Caio[C, L, A])(f: L => L): Caio[C, L, A] =
    fa.censor(f)(monoid)
}

object CaioCensor {
  def apply[C, L](implicit M: Monoid[L]): CaioCensor[C, L] =
    new CaioCensor[C, L] {
      val monoid: Monoid[L]                                  = M
      val applicative: CommutativeApplicative[Caio[C, L, _]] = CaioApplicative[C, L]
    }
}
