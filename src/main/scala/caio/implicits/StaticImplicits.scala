package caio.implicits

import caio.Caio
import caio.mtl.ApplicativeFail
import caio.std._
import cats.effect.{Async, Concurrent, ContextShift, IO, Sync}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, MonadState}
import cats.{Monad, Monoid, Parallel}

class StaticImplicits[C, V, L](implicit ML: Monoid[L]){

  implicit val staticCaioMonad: Monad[Caio[C,V,L, *]] =
    new CaioMonad[C, V, L]

  implicit val staticCaioSync: Sync[Caio[C,V,L, *]] =
    new CaioSync[C, V, L]()(ML)

  implicit val staticCaioAsync: Async[Caio[C,V,L, *]] =
    new CaioAsync[C, V, L]()(ML)

  implicit def staticCaioConcurrent(implicit CS: ContextShift[IO]): Concurrent[Caio[C,V,L, *]] =
    new CaioConcurrent[C, V, L]()(ML, CS)

  implicit def staticCaioContextShift(implicit CS: ContextShift[IO]): ContextShift[Caio[C,V,L, *]] =
    new CaioContextShift[C, V, L]()(ML, CS)

  implicit def staticCaioParallel(implicit CS: ContextShift[IO]): Parallel[Caio[C,V,L, *]] =
    new CaioParallel[C, V, L]()(ML, CS)

  implicit val staticCaioApplicativeFail: ApplicativeFail[Caio[C, V, L, *], V] =
    new CaioApplicativeFail[C, V, L]

  implicit val staticCaioApplicativeCensor: ApplicativeCensor[Caio[C, V, L, *], L] =
    new CaioApplicativeCensor[C, V, L]()(ML)

  implicit val staticCaioApplicativeAsk: ApplicativeAsk[Caio[C, V, L, *], C] =
    new CaioApplicativeAsk[C, V, L]

  implicit val staticCaioMonadState: MonadState[Caio[C, V, L, *], C] =
    new CaioMonadState[C, V, L]
}
