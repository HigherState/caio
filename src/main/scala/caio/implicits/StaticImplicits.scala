package caio.implicits

import caio.Caio
import caio.mtl.ApplicativeFail
import caio.std._
import cats.effect.{Async, Concurrent, ContextShift, IO}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, MonadState}
import cats.Monoid

class StaticImplicits[C, V, L](implicit M:Monoid[L]) {

  implicit val staticCaioFunctor:Async[Caio[C,V,L, *]] =
    new CaioAsync[C, V, L]

  implicit def staticCaioConcurrent(implicit CS:ContextShift[IO]):Concurrent[Caio[C,V,L, *]] =
    new CaioConcurrent[C, V, L]

  implicit val staticCaioApplicativeFail:ApplicativeFail[Caio[C, V, L, *], V] =
    new CaioApplicativeFail[C, V, L]

  implicit val staticCaioApplicativeCensor:ApplicativeCensor[Caio[C, V, L, *], L] =
    new CaioApplicativeCensor[C, V, L]

  implicit val staticCaioApplicativeAsk:ApplicativeAsk[Caio[C, V, L, *], C] =
    new CaioApplicativeAsk[C, V, L]

  implicit val staticCaioMonadState:MonadState[Caio[C, V, L, *], C] =
    new CaioMonadState[C, V, L]
}
