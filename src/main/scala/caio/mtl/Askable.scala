package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

trait Askable[F[_], E] {
  type FE[A]

  def applicativeAsk[E2](M:Mixer[E, E2]):ApplicativeAsk[FE, E2]

  def monadState[E2](M:Mixer[E, E2]):MonadState[FE, E2]

  def extender[E2](M:Mixer[E, E2]):Extender[FE, E]

}
