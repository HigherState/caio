package caio.implicits

import caio.Caio
import caio.mtl.{ApplicativeFail, InvariantAsk}
import caio.std._
import cats.effect.{Async, Concurrent, ContextShift, IO, Sync}
import cats.mtl.{Censor, Local, Stateful}
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

  implicit val staticCaioCensor: Censor[Caio[C, V, L, *], L] =
    new CaioCensor[C, V, L]()(ML)

  implicit val staticCaioAsk: InvariantAsk[Caio[C, V, L, *], C] =
    new CaioAsk[C, V, L]

  implicit val staticCaioLocal: Local[Caio[C, V, L, *], C] =
    new CaioLocal[C, V, L]

  implicit val staticCaioStateful: Stateful[Caio[C, V, L, *], C] =
    new CaioStateful[C, V, L]
}
