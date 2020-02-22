package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

trait Contextual {

  implicit def toAskableAux[F[_], E](implicit E:Askable[F, E]): Asked[E.FE, F, E] =
    E.asInstanceOf[Asked[E.FE, F, E]]

  //E2 is a subset of E
  implicit def extenderFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):Extender[FE, E] =
    P.extender[E2](M)

  implicit def applicativeAskFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):ApplicativeAsk[FE, E2] =
    new MixedApplicativeAsk(P.applicativeAsk, M)

  implicit def monadStateFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):MonadState[FE, E2] =
    new MixedMonadState(P.monadState, M)

}

object Contextual
  extends Contextual
  with ContextualConcurrent
  with ContextualWriter
  with ContextualFail
