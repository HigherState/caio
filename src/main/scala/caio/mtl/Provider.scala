package caio.mtl

import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait Provider[F[_]] {
  def apply[E]:Provides[F, E]
}

trait Extender[F[_], E1] {
  def apply[E2](implicit M:Mixer[(E1, E2), E2], EV: E1 =:!= E2):Extends[F, E1, E2]

  def applicativeAsk:ApplicativeAsk[F, E1]

  def monadState:MonadState[F, E1]
}

trait Provides[F[_], E] extends Askable[F, E] {

  def apply[A](e:E)(f:FE[A]):F[A]

  def lift[A](f:F[A]):FE[A]

  def lift[G[_], A](f:FunctionK[G, F[A]]):FunctionK[G, FE[A]]
}

trait Extends[F[_], E1, E2] extends Askable[F, (E1, E2)] {

  def apply[A](c:E2)(f:FE[A]):F[A]

  def lift[A](f:F[A]):FE[A]

  def lift[G[_], A](f:FunctionK[G, F[A]]):FunctionK[G, FE[A]]
}


object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}