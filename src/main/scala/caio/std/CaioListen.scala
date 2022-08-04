package caio.std

import caio.Caio
import cats.mtl.Listen

class CaioListen[C, L] extends CaioTell[C, L] with Listen[Caio[C, L, *], L] {
  def listen[A](fa: Caio[C, L, A]): Caio[C, L, (A, L)] =
    fa.listen

  override def listens[A, B](fa: Caio[C, L, A])(f: L => B): Caio[C, L, (A, B)] =
    fa.listen.map { case (a, l) => a -> f(l) }
}
