package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

trait Contextual {

  implicit def toAskableAux[F[_], E](implicit E:Askable[F, E]): Asked[E.FE, F, E] =
    E.asInstanceOf[Asked[E.FE, F, E]]

  implicit def extenderFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):Extender[FE, E] =
    P.extender[E2](M)

  implicit def applicativeAskFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):ApplicativeAsk[FE, E2] =
    P.applicativeAsk[E2](M)

  implicit def monadStateFE[F[_], FE[_], E, E2](implicit P:Asked[FE, F, E], M:Mixer[E, E2]):MonadState[FE, E2] =
    P.monadState[E2](M)

}

// Only three levels of inheritance can be used without implicit diverging

object Contextual
  extends Contextual
  with ContextualApplicative
  with ContextualFunctor
  with ContextualMonad
  with ContextualMonadError
  with ContextualBracket
  with ContextualSync
  with ContextualLiftIO
  with ContextualAsync
  with ContextualConcurrent
  with ContextualWriter
