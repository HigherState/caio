package caio.implicits

import caio.Caio
import caio.mtl.InvariantAsk
import caio.std._
import cats.effect.{Async, Concurrent, ContextShift, IO, Sync}
import cats.mtl.{Censor, Local, Stateful}
import cats.{Monad, Monoid, Parallel}

class StaticImplicits[C, L](implicit ML: Monoid[L]) {

  implicit val staticCaioMonad: Monad[Caio[C, L, *]] =
    new CaioMonad[C, L]

  implicit val staticCaioSync: Sync[Caio[C, L, *]] =
    new CaioSync[C, L]()(ML)

  implicit val staticCaioAsync: Async[Caio[C, L, *]] =
    new CaioAsync[C, L]()(ML)

  implicit def staticCaioConcurrent(implicit CS: ContextShift[IO]): Concurrent[Caio[C, L, *]] =
    new CaioConcurrent[C, L]()(ML, CS)

  implicit def staticCaioContextShift(implicit CS: ContextShift[IO]): ContextShift[Caio[C, L, *]] =
    new CaioContextShift[C, L]()(ML, CS)

  implicit def staticCaioParallel(implicit CS: ContextShift[IO]): Parallel[Caio[C, L, *]] =
    new CaioParallel[C, L]()(ML, CS)

  implicit val staticCaioCensor: Censor[Caio[C, L, *], L] =
    new CaioCensor[C, L]()(ML)

  implicit val staticCaioAsk: InvariantAsk[Caio[C, L, *], C] =
    new CaioAsk[C, L]

  implicit val staticCaioLocal: Local[Caio[C, L, *], C] =
    new CaioLocal[C, L]

  implicit val staticCaioStateful: Stateful[Caio[C, L, *], C] =
    new CaioStateful[C, L]
}
