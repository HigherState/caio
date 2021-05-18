package caio

import cats.arrow.FunctionK

trait BijectionK[F[_], G[_]] extends FunctionK[F, G] {

  def unapply[A](fa: G[A]): F[A]

  def invert: FunctionK[G, F]
}
