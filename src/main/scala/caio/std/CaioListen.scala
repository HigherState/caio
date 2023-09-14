package caio.std

import caio.Caio
import cats.Monoid
import cats.mtl.Listen

trait CaioListen[C, L] extends CaioTell[C, L] with Listen[Caio[C, L, _], L] {
  def listen[A](fa: Caio[C, L, A]): Caio[C, L, (A, L)] =
    fa.listen(monoid)
}

object CaioListen {
  def apply[C, L](implicit M: Monoid[L]): CaioListen[C, L] =
    new CaioListen[C, L] { val monoid: Monoid[L] = M }
}
