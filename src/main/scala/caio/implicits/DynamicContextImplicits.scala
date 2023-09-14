package caio.implicits

import caio.Caio
import caio.mtl.{Effectful, InvariantAsk}
import caio.std.{CaioAsk, CaioAsync, CaioConcurrent, CaioEffectful, CaioLiftIO, CaioLocal, CaioStateful, CaioSync}
import cats.effect.std.Dispatcher
import cats.{CommutativeMonad, Monoid, Parallel}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.mtl.{Censor, Local, Stateful}

class DynamicContextImplicits[L] {
  private val static: StaticImplicits[Unit, L]                      =
    new StaticImplicits[Unit, L] {}

  implicit def dynamicCaioMonad[C]: CommutativeMonad[Caio[C, L, _]] =
    static.staticCaioMonad.asInstanceOf[CommutativeMonad[Caio[C, L, _]]]

  implicit def dynamicCaioSync[C]: Sync[Caio[C, L, _]] =
    CaioSync[C, L]

  implicit def dynamicCaioAsync[C]: Async[Caio[C, L, _]] =
    CaioAsync[C, L]

  implicit def dynamicCaioConcurrent[C]: Concurrent[Caio[C, L, _]] =
    CaioConcurrent[C, L]

  implicit def dynamicCaioCensor[C](implicit ML: Monoid[L]): Censor[Caio[C, L, _], L] =
    static.staticCaioCensor.asInstanceOf[Censor[Caio[C, L, _], L]]

  implicit def dynamicCaioAsk[C]: InvariantAsk[Caio[C, L, _], C] =
    CaioAsk[C, L]

  implicit def dynamicCaioLocal[C]: Local[Caio[C, L, _], C] =
    CaioLocal[C, L]

  implicit def dynamicCaioStateful[C]: Stateful[Caio[C, L, _], C] =
    CaioStateful[C, L]

  implicit def dynamicLiftIO[C]: LiftIO[Caio[C, L, _]] =
    CaioLiftIO[C, L]

  implicit def dynamicCaioParallel[C]: Parallel.Aux[Caio[C, L, _], Caio.Par[C, L, _]] =
    static.staticCaioParallel.asInstanceOf[Parallel.Aux[Caio[C, L, _], Caio.Par[C, L, _]]]

  implicit def dynamicCaioEffectful[C](implicit D: Dispatcher[Caio[Unit, L, _]]): Effectful[Caio[C, L, _]] =
    new CaioEffectful[C, L](dynamicCaioLocal[C], D)
}
