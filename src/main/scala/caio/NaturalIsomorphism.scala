package caio

import cats.~>

trait NaturalIsomorphism[F[_], G[_]] extends (F ~> G)  {
  def apply[A](fa: F[A]): G[A]

  def unapply[A](fa: G[A]):F[A]

  def invert:(G ~> F)
}
