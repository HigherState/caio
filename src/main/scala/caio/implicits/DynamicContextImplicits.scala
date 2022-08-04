package caio.implicits

import caio.Caio
import caio.mtl.{Effectful, InvariantAsk}
import caio.std.{
  CaioAsk,
  CaioAsync,
  CaioConcurrent,
  CaioContextShift,
  CaioEffectful,
  CaioLocal,
  CaioParallel,
  CaioStateful,
  CaioSync
}
import cats.{Monad, Monoid, Parallel}
import cats.effect.{Async, Concurrent, ConcurrentEffect, ContextShift, IO, Sync}
import cats.mtl.{Censor, Local, Stateful}

class DynamicContextImplicits[L](implicit ML: Monoid[L]) {

  private val static: StaticImplicits[Unit, L]           =
    new StaticImplicits[Unit, L]()(ML) {}

  implicit def dynamicCaioMonad[C]: Monad[Caio[C, L, *]] =
    static.staticCaioMonad.asInstanceOf[Monad[Caio[C, L, *]]]

  implicit def dynamicCaioSync[C]: Sync[Caio[C, L, *]] =
    new CaioSync[C, L]()(ML)

  implicit def dynamicCaioAsync[C]: Async[Caio[C, L, *]] =
    new CaioAsync[C, L]()(ML)

  implicit def dynamicCaioConcurrent[C](implicit CS: ContextShift[IO]): Concurrent[Caio[C, L, *]] =
    new CaioConcurrent[C, L]()(ML, CS)

  implicit def dynamicCaioContextShift[C](implicit CS: ContextShift[IO]): ContextShift[Caio[C, L, *]] =
    new CaioContextShift[C, L]()(ML, CS)

  implicit def dynamicCaioParallel[C](implicit CS: ContextShift[IO]): Parallel[Caio[C, L, *]] =
    new CaioParallel[C, L]()(ML, CS)

  implicit def dynamicCaioCensor[C]: Censor[Caio[C, L, *], L] =
    static.staticCaioCensor.asInstanceOf[Censor[Caio[C, L, *], L]]

  implicit def dynamicCaioAsk[C]: InvariantAsk[Caio[C, L, *], C] =
    new CaioAsk[C, L]

  implicit def dynamicCaioLocal[C]: Local[Caio[C, L, *], C] =
    new CaioLocal[C, L]

  implicit def dynamicCaioStateful[C]: Stateful[Caio[C, L, *], C] =
    new CaioStateful[C, L]

  implicit def dynamicCaioEffectful[C](implicit CE: ConcurrentEffect[Caio[Unit, L, *]]): Effectful[Caio[C, L, *]] =
    new CaioEffectful[C, L](dynamicCaioLocal[C], CE)

}
