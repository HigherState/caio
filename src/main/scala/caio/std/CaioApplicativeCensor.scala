package caio.std

import caio.{Caio, LogMapCaio}
import cats.mtl.ApplicativeCensor
import cats.{Applicative, Monoid}

class CaioApplicativeCensor[C, V, L:Monoid] extends CaioFunctorListen[C, V, L] with ApplicativeCensor[Caio[C, V, L, *], L] {

  val applicative: Applicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  val monoid: Monoid[L] =
    implicitly[Monoid[L]]

  def censor[A](fa: Caio[C, V, L, A])(f: L => L): Caio[C, V, L, A] =
    LogMapCaio(fa, f)

  def clear[A](fa: Caio[C, V, L, A]): Caio[C, V, L, A] =
    censor(fa)(_ => implicitly[Monoid[L]].empty)
}
