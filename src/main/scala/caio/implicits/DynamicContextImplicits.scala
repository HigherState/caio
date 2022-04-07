package caio.implicits

import caio.Caio
import caio.mtl.InvariantAsk
import caio.std.{CaioAsk, CaioAsync, CaioConcurrent, CaioLiftIO, CaioLocal, CaioStateful, CaioSync}
import cats.{CommutativeMonad, Monoid, Parallel}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.mtl.{Censor, Local, Stateful}

class DynamicContextImplicits[L] {
  private val static: StaticImplicits[Unit, L]                      =
    new StaticImplicits[Unit, L] {}

  implicit def dynamicCaioMonad[C]: CommutativeMonad[Caio[C, L, *]] =
    static.staticCaioMonad.asInstanceOf[CommutativeMonad[Caio[C, L, *]]]

  implicit def dynamicCaioSync[C]: Sync[Caio[C, L, *]] =
    CaioSync[C, L]

  implicit def dynamicCaioAsync[C]: Async[Caio[C, L, *]] =
    CaioAsync[C, L]

  implicit def dynamicCaioConcurrent[C]: Concurrent[Caio[C, L, *]] =
    CaioConcurrent[C, L]

  implicit def dynamicCaioCensor[C](implicit ML: Monoid[L]): Censor[Caio[C, L, *], L] =
    static.staticCaioCensor.asInstanceOf[Censor[Caio[C, L, *], L]]

  implicit def dynamicCaioAsk[C]: InvariantAsk[Caio[C, L, *], C] =
    CaioAsk[C, L]

  implicit def dynamicCaioLocal[C]: Local[Caio[C, L, *], C] =
    CaioLocal[C, L]

  implicit def dynamicCaioStateful[C]: Stateful[Caio[C, L, *], C] =
    CaioStateful[C, L]

  implicit def dynamicLiftIO[C]: LiftIO[Caio[C, L, *]] =
    CaioLiftIO[C, L]

  implicit def dynamicCaioParallel[C]: Parallel.Aux[Caio[C, L, *], Caio.Par[C, L, *]] =
    static.staticCaioParallel.asInstanceOf[Parallel.Aux[Caio[C, L, *], Caio.Par[C, L, *]]]
}
