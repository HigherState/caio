package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait Provider[F[_]] {

  def apply[E]:Provides[F, E]
}
//TODO Can nearly inherit from Applicative ask, and remove applicateAsk from contextual but it diverges
trait Extender[F[_], E1] {
  def apply[E2](implicit M:Mixer[(E1, E2), E2], EV: E1 =:!= E2):Extends[F, E1, E2]
}

sealed trait Askable[F[_], E] {
  type FE[A]

  def applicativeAsk[E2](M:Mixer[E, E2]):ApplicativeAsk[FE, E2]

  def monadState[E2](M:Mixer[E, E2]):MonadState[FE, E2]

  def extender[E2](M:Mixer[E, E2]):Extender[FE, E]
}

trait Provides[F[_], E] extends Askable[F, E] {

  def apply[A](e:E)(f:FE[A]):F[A]
}


trait Extends[F[_], E1, E2] extends Askable[F, (E1, E2)] {

  def apply[A](c:E2)(f:FE[A]):F[A]
}