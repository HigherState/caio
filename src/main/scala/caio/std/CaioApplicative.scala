package caio.std

import caio.{Caio, CaioPure}
import cats.Applicative

trait CaioApplicative extends Applicative[Caio]{
  def pure[A](x: A): Caio[A] =
    CaioPure(x)

  override def ap[A, B](ff: Caio[A => B])(fa: Caio[A]): Caio[B] =
    fa.flatMap(a => ff.map(_(a)))
}
