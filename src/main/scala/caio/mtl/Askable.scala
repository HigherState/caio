package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

trait Askable[F[_], E] extends ContextTransformers[F] {
  type FE[A]

  def applicativeAsk:ApplicativeAsk[FE, E]

  def monadState:MonadState[FE, E]

  def extender[E2](implicit M:Mixer[E, E2]):Extender[FE, E2]

}
