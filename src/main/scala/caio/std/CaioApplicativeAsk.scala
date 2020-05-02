package caio.std

import caio._
import cats.{Applicative, Monoid}
import cats.mtl.ApplicativeAsk

class CaioApplicativeAsk[C, V, L:Monoid] extends ApplicativeAsk[Caio[C, V, L, *], C] {
  val applicative: Applicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def ask: Caio[C, V, L, C] =
    CaioKleisli{c => SuccessResult[C, V, L, C](c, EmptyStore)}

  def reader[A](f: C => A): Caio[C, V, L, A] =
    ask.map(f)
}

