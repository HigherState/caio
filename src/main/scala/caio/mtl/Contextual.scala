package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer

trait Contextual {

  implicit def toExtendsAux[F[_], E1, E2](implicit E:Extends[F, E1, E2]): Extended[E.FE, F, E1, E2] =
    E.asInstanceOf[Extended[E.FE, F, E1, E2]]

  implicit def extendedToExtender[F[_], FE[_], E1, E2, E3](implicit P:Extended[FE, F, E1, E2], M:Mixer[(E1, E2), E3], I:Mixer[(E3, Unit), (E1, E2)]):Extender[FE, E3] =
    P.extender[E3]

  implicit def extenderToApplicativeAsk[F[_], FE[_], E1, E2, E3](implicit P:Extended[FE, F, E1, E2], M:Mixer[(E1, E2), E3]):ApplicativeAsk[FE, E3] =
    new MixedApplicativeAsk(P.applicativeAsk)

  implicit def extenderToMonadState[F[_], FE[_], E1, E2, E3](implicit P:Extended[FE, F, E1, E2], M:Mixer[(E1, E2), E3]):MonadState[FE, E3] =
    new MixedMonadState(P.monadState)
}

object Contextual
  extends Contextual
  with ContextualConcurrent
  with ContextualWriter
  with ContextualFail
