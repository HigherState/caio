package caio.mtl

import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

//Issue with Mixer[(String), (Unit, String)

trait Extender[F[_], E1] {
  def apply[E2](implicit EV: E1 =:!= E2):Extends[F, E1, E2]

  def applicativeAsk:ApplicativeAsk[F, E1]

  def monadState:MonadState[F, E1]
}


trait Extends[F[_], E1, E2] extends ContextTransformers[F] {

  type FE[A]

  def apply[A](c:E2):FunctionK[FE, F]

  def unapply:FunctionK[F, FE]

  def applicativeAsk:ApplicativeAsk[FE, (E1, E2)]

  def monadState:MonadState[FE, (E1, E2)]

  // E3 has to be purely a recombination of E1, and E2 otherwise we cannot perform an unapply
  // Inverse includes Unit so we can map back when E1 may be Unit
  def extender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]):Extender[FE, E3]

  //def function[E3](implicit M: Mixer[(E1, E2), E3]):Extender[FE, E3]
}


object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}