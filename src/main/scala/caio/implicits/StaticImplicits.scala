package caio.implicits

import caio.Caio
import caio.mtl.ApplicativeFail
import caio.std._
import cats.effect.{Async, Concurrent, ContextShift, IO, Sync}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, MonadState}
import cats.{Monad, Monoid}

class StaticImplicits[C, V, L:Monoid]{

  implicit val staticCaioMonad:Monad[Caio[C,V,L, *]] =
    new CaioMonad[C, V, L]

  implicit val staticCaioSync:Sync[Caio[C,V,L, *]] =
    new CaioSync[C, V, L]

  implicit val staticCaioAsync:Async[Caio[C,V,L, *]] =
    new CaioAsync[C, V, L]

  implicit def staticCaioConcurrent(implicit CS:ContextShift[IO]):Concurrent[Caio[C,V,L, *]] =
    new CaioConcurrent[C, V, L]

  implicit def staticCaioContextShift(implicit CS:ContextShift[IO]):ContextShift[Caio[C,V,L, *]] =
    new CaioContextShift[C, V, L]

  implicit val staticCaioApplicativeFail:ApplicativeFail[Caio[C, V, L, *], V] =
    new CaioApplicativeFail[C, V, L]

  implicit val staticCaioApplicativeCensor:ApplicativeCensor[Caio[C, V, L, *], L] =
    new CaioApplicativeCensor[C, V, L]

  implicit val staticCaioApplicativeAsk:ApplicativeAsk[Caio[C, V, L, *], C] =
    new CaioApplicativeAsk[C, V, L]

  implicit val staticCaioMonadState:MonadState[Caio[C, V, L, *], C] =
    new CaioMonadState[C, V, L]
}
