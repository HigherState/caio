package caio.mtl

import cats.Monad
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait AskProjector {

  implicit def askProjection[M[_], A, B]
    (implicit AA:ApplicativeAsk[M, A], M:Mixer[A, B], EV: A =:!= B): ApplicativeAsk[M, B] =
      AskProjection(AA, M).apply()

}

object AskProjector extends AskProjector
