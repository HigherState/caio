package caio.mtl

import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import shapeless.=:!=

trait Extender[F[_], E1] {
  def apply[E2](implicit EV: E1 =:!= E2):Extends[F, E1, E2]

  def applicativeAsk:ApplicativeAsk[F, E1]

  def monadState:MonadState[F, E1]
}


trait Extends[F[_], E1, E2] extends Askable[F, (E1, E2)] {

  def apply[A](c:E2)(f:FE[A]):F[A]

  def functionK:FunctionK[F, FE]
}


object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}