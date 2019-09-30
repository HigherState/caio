package caio.std

import caio._
import cats.Applicative
import cats.mtl.ApplicativeAsk

import scala.reflect.ClassTag

class CaioApplicativeAsk[E](implicit T:ClassTag[E]) extends ApplicativeAsk[Caio, E] {
  val applicative: Applicative[Caio] =
    new CaioApplicative {}

  def ask: Caio[E] = CaioKleisli{c =>
    c.get(T).fold[PureResult[E]](ErrorResult(ContextNotFoundException(T), Store.empty)){ e =>
      SuccessResult(e, Store.empty)
    }
  }

  def reader[A](f: E => A): Caio[A] =
    ask.map(f)
}
