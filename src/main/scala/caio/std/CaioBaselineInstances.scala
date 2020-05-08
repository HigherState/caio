package caio.std

import caio.mtl.ApplicativeFail
import caio.Caio
import cats.{Eq, Monoid}
import cats.data.NonEmptyList
import cats.effect.{ConcurrentEffect, ContextShift, IO}
import cats.mtl.ApplicativeCensor

class CaioBaselineInstances[V, L:Monoid:Eq]
  (onSuccess:L => IO[Unit])
  (onError:(Throwable, L) => IO[Unit])
  (onFailure:(NonEmptyList[V], L) => IO[Unit])
  (implicit CS:ContextShift[IO]){

  type Cai0[A] = Caio[Unit, V, L, A]

  implicit val baselineConcurrentEffect:ConcurrentEffect[Cai0] =
    new CaioConcurrentEffect[Unit, V, L](())((_ , l:L) => onSuccess(l))((e, _, l:L) => onError(e, l))((f, _, l:L) => onFailure(f, l))

  implicit val baselineApplicativeCensor:ApplicativeCensor[Cai0, L] =
    new CaioApplicativeCensor[Unit, V, L]

  implicit val baselineApplicativeFail:ApplicativeFail[Cai0, V] =
    new CaioApplicativeFail[Unit, V, L]

//  implicit val baselineContext:Context[Cai0, Unit, L, Throwable, V] =
//    new CaioContext[Unit, V, L]

}
