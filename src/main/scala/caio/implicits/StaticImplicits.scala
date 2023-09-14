package caio.implicits

import caio.Caio
import caio.mtl.InvariantAsk
import caio.std._
import cats.{CommutativeMonad, Monoid, Parallel}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.effect.instances.spawn
import cats.mtl.{Censor, Local, Stateful}

class StaticImplicits[C, L] {
  implicit val staticCaioMonad: CommutativeMonad[Caio[C, L, _]] =
    CaioMonad[C, L]

  implicit val staticCaioSync: Sync[Caio[C, L, _]] =
    CaioSync[C, L]

  implicit val staticCaioAsync: Async[Caio[C, L, _]] =
    CaioAsync[C, L]

  implicit val staticCaioConcurrent: Concurrent[Caio[C, L, _]] =
    CaioConcurrent[C, L]

  implicit def staticCaioCensor(implicit M: Monoid[L]): Censor[Caio[C, L, _], L] =
    CaioCensor[C, L]

  implicit val staticCaioAsk: InvariantAsk[Caio[C, L, _], C] =
    CaioAsk[C, L]

  implicit val staticCaioLocal: Local[Caio[C, L, _], C] =
    CaioLocal[C, L]

  implicit val staticCaioStateful: Stateful[Caio[C, L, _], C] =
    CaioStateful[C, L]

  implicit val staticLiftIO: LiftIO[Caio[C, L, _]] =
    CaioLiftIO[C, L]

  implicit val staticCaioParallel: Parallel.Aux[Caio[C, L, _], Caio.Par[C, L, _]] =
    spawn.parallelForGenSpawn[Caio[C, L, _], Throwable]
}
