package caio.std

import caio.Caio
import cats.mtl.{Censor, Listen, Tell}

trait CaioTell[C, L] extends Tell[Caio[C, L, *], L] {

  def tell(l: L): Caio[C, L, Unit]             =
    Caio.tell(l)

  override def writer[A](a: A, l: L): Caio[C, L, A] =
    Caio.tell(l).as(a)

  override def tuple[A](ta: (L, A)): Caio[C, L, A] =
    writer(ta._2, ta._1)
}

trait CaioListen[C, L] extends CaioTell[C, L] with Listen[Caio[C, L, *], L] {
  def listen[A](fa: Caio[C, L, A]): Caio[C, L, (A, L)] =
    fa.listen
}

trait CaioCensor[C, L] extends CaioListen[C, L] with CaioMonoid[L] with Censor[Caio[C, L, *], L] {

  def censor[A](fa: Caio[C, L, A])(f: L => L): Caio[C, L, A] =
    fa.censor(f)
}

