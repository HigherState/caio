package caio.mtl

import caio.<~>
import cats.{Applicative, Functor, Monad, MonadError, ~>}
import cats.effect.{Async, Bracket, Concurrent, ConcurrentEffect, LiftIO, Sync}
import cats.mtl.{Censor, Listen, Tell, Stateful}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait Extender[F[_], E1] {

  def apply[E2](implicit EV: E1 =:!= E2): Extends[F, E1, E2]

  def applicativeAsk: InvariantAsk[F, E1]

  def state: Stateful[F, E1]

}

trait ExtendsOn[F[_], Source[_], Context] extends Extender[F, Context] {

  def functionK: Source ~> F

  def bijectionK(e2: Context): Source <~> F
}


trait Extends[F[_], E1, E2] {

  type FE[A]

  def concurrentEffect(c:E2)(implicit CE:ConcurrentEffect[F]):ConcurrentEffect[FE] =
    new ConcurrentEffectIsomorphism[FE, F](CE, apply(c))

  def ask: InvariantAsk[FE, (E1, E2)]

  def stateful: Stateful[FE, (E1, E2)]

  // E3 has to be purely a recombination of E1, and E2 otherwise we cannot perform an unapply
  // Inverse includes Unit so we can map back when E1 may be Unit
  def extender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]):Extender[FE, E3]

  def functionK: F ~> FE

  def apply(e2: E2): FE <~> F

  implicit def extendedToExtender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]): Extender[FE, E3] =
    this.extender[E3]

  implicit def extenderToAsk[E3](implicit M: Mixer[(E1, E2), E3]): InvariantAsk[FE, E3] =
    new MixedAsk(this.ask)

  implicit def extenderToStateful[E3](implicit M: Mixer[(E1, E2), E3]): Stateful[FE, E3] =
    new MixedStateful(this.stateful)

  implicit def transformApplicative(implicit A: Applicative[F]): Applicative[FE]

  implicit def transformFunctor(implicit F: Functor[F]): Functor[FE]

  implicit def transformMonad(implicit M: Monad[F]): Monad[FE]

  implicit def transformMonadError[E](implicit M: MonadError[F, E]): MonadError[FE, E]

  implicit def transformBracket[E](implicit M: Bracket[F, E]): Bracket[FE, E]

  implicit def transformSync(implicit S: Sync[F]): Sync[FE]

  implicit def transformAsync(implicit A: Async[F]): Async[FE]

  implicit def transformLiftIO(implicit L: LiftIO[F]): LiftIO[FE]

  implicit def transformConcurrent(implicit C: Concurrent[F]): Concurrent[FE]

  implicit def transformApplicativeFail[V](implicit A: ApplicativeFail[F, V]): ApplicativeFail[FE, V]

  implicit def transformTell[L](implicit F: Tell[F, L]): Tell[FE, L]

  implicit def transformListen[L](implicit F: Listen[F, L]): Listen[FE, L]

  implicit def transformCensor[L](implicit F: Censor[F, L]): Censor[FE, L]
}

object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}