package caio.mtl

import caio.<~>
import cats.{Applicative, Functor, Monad, MonadError, ~>}
import cats.effect.{Async, Bracket, Concurrent, ConcurrentEffect, LiftIO, Sync}
import cats.mtl.{ApplicativeAsk, ApplicativeCensor, FunctorListen, FunctorTell, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

/**
 * There are 2 cases of extending a monad context.
 * @tparam F
 * @tparam E1
 */
trait Extender[F[_], E1] {

  def apply[E2](implicit EV: E1 =:!= E2):Extends[F, E1, E2]

  def applicativeAsk:ApplicativeAsk[F, E1]

  def monadState:MonadState[F, E1]

}

trait ExtendsOn[F[_], Source[_], Context] extends Extender[F, Context] {

  def functionK: Source ~> F

  def bijectionK(e2: Context): Source <~> F
}


trait Extends[F[_], E1, E2] {

  type FE[A]

  def concurrentEffect(c:E2)(implicit CE:ConcurrentEffect[F]):ConcurrentEffect[FE] =
    new ConcurrentEffectIsomorphism[FE, F](CE, apply(c))

  def applicativeAsk:ApplicativeAsk[FE, (E1, E2)]

  def monadState:MonadState[FE, (E1, E2)]

  // E3 has to be purely a recombination of E1, and E2 otherwise we cannot perform an unapply
  // Inverse includes Unit so we can map back when E1 may be Unit
  def extender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]):Extender[FE, E3]

  def functionK: F ~> FE

  def apply(e2: E2): FE <~> F


  implicit def extendedToExtender[E3](implicit M:Mixer[(E1, E2), E3], I:Mixer[(E3, Unit), (E1, E2)]):Extender[FE, E3] =
    this.extender[E3]

  implicit def extenderToApplicativeAsk[E3](implicit M:Mixer[(E1, E2), E3]):ApplicativeAsk[FE, E3] =
    new MixedApplicativeAsk(this.applicativeAsk)

  implicit def extenderToMonadState[E3](implicit M:Mixer[(E1, E2), E3]):MonadState[FE, E3] =
    new MixedMonadState(this.monadState)

  implicit def transformApplicative(implicit A:Applicative[F]):Applicative[FE]

  implicit def transformFunctor(implicit F:Functor[F]):Functor[FE]

  implicit def transformMonad(implicit M:Monad[F]):Monad[FE]

  implicit def transformMonadError[E](implicit M:MonadError[F, E]):MonadError[FE, E]

  implicit def transformBracket[E](implicit M:Bracket[F, E]):Bracket[FE, E]

  implicit def transformSync(implicit S:Sync[F]):Sync[FE]

  implicit def transformAsync(implicit A:Async[F]):Async[FE]

  implicit def transformLiftIO(implicit L:LiftIO[F]):LiftIO[FE]

  implicit def transformConcurrent(implicit C:Concurrent[F]):Concurrent[FE]

  implicit def transformApplicativeFail[V](implicit A:ApplicativeFail[F, V]):ApplicativeFail[FE, V]

  implicit def transformFunctorTell[L](implicit F:FunctorTell[F, L]):FunctorTell[FE, L]

  implicit def transformFunctorListen[L](implicit F:FunctorListen[F, L]):FunctorListen[FE, L]

  implicit def transformApplicativeCensor[L](implicit F:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L]
}

object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}