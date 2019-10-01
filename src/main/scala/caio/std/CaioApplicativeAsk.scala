package caio.std

import caio._
import cats.{Applicative, Monoid}
import cats.mtl.ApplicativeAsk

import scala.reflect.ClassTag

class CaioApplicativeAsk[C, V, L:Monoid, E](implicit T:ClassTag[E]) extends ApplicativeAsk[Caio[C, V, L, *], E] {
  val applicative: Applicative[Caio[C, V, L, *]] =
    new CaioApplicative[C, V, L]

  def ask: Caio[C, V, L, E] =
    CaioKleisli{c => SuccessResult(c, Store.empty)}

  def reader[A](f: E => A): Caio[C, V, L, A] =
    ask.map(f)
}
