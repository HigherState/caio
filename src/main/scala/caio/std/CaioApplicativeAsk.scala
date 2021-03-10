package caio.std

import caio.Caio
import cats.CommutativeApplicative
import cats.mtl.ApplicativeAsk

class CaioApplicativeAsk[C, V, L] extends ApplicativeAsk[Caio[C, V, L, *], C] {
  val applicative: CommutativeApplicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def ask: Caio[C, V, L, C] =
    Caio.getContext

  def reader[A](f: C => A): Caio[C, V, L, A] =
    Caio.getContext.map(f)
}

