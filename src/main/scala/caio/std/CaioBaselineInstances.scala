package caio.std

import caio.mtl.{ApplicativeFail, Context}
import caio.{Caio, ErrorOrFailure}
import cats.Monoid
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import cats.mtl.ApplicativeCensor

class CaioBaselineInstances[V, L:Monoid]
  (onSuccess:L => IO[Unit])
  (onFailure:(ErrorOrFailure[V], L) => IO[Unit])
  (implicit CS:ContextShift[IO]){

  type Cai0[A] = Caio[Unit, V, L, A]

  implicit val baselineConcurrentEffect:ConcurrentEffect[Cai0] =
    new CaioConcurrentEffect[Unit, V, L](())((_ , l:L) => onSuccess(l))((e:ErrorOrFailure[V], _, l:L) => onFailure(e, l))

  implicit val baselineApplicativeCensor:ApplicativeCensor[Cai0, L] =
    new CaioApplicativeCensor[Unit, V, L]

  implicit val baselineApplicativeFail:ApplicativeFail[Cai0, V] =
    new CaioApplicativeFail[Unit, V, L]

//  implicit val baselineContext:Context[Cai0, Unit, L, Throwable, V] =
//    new CaioContext[Unit, V, L]

}
