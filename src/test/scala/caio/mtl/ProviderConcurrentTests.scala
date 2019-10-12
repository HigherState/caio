package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError}
import cats.effect.{Async, Concurrent, LiftIO, Sync}
import cats.mtl.{ApplicativeAsk, MonadState}

class ProviderConcurrentTests {

  class StateFunctor[M[_]:MonadState[*[_], Int]:Functor] {
    def run:M[Int] = MonadState[M,Int].get
  }

  class StateApplicative[M[_]:MonadState[*[_], Int]:Applicative] {
    def run:M[Int] = MonadState[M,Int].get
  }

  class StateMonad[M[_]:MonadState[*[_], Int]:Monad] {
    def run:M[Int] = MonadState[M,Int].get
  }

  class AskMonadError[M[_]:ApplicativeAsk[*[_], Int]:MonadError[*[_], Throwable]] {
    def run:M[Int] = ApplicativeAsk[M, Int].ask
  }

//  class StateLiftIO[M[_]:MonadState[*[_], Int]:LiftIO] {
//    def run:M[Int] = MonadState[M,Int].get
//  }
//
//  class StateConcurrent[M[_]:MonadState[*[_], Int]:Concurrent] {
//    def run:M[Int] = MonadState[M,Int].get
//  }

  class FunctorCheck[M[_]:Provider:Functor] {
    val functor = implicitly[Functor[M]]

    import Contextual._
    implicit val E = Provider[M].apply[Int]

    val stateFunctor = new StateFunctor[E.FE]
  }

  class ApplicativeCheck[M[_]:Provider:Applicative] {
    val applicative = implicitly[Applicative[M]]

    import Contextual._
    implicit val E = Provider[M].apply[Int]

    val stateApplicative = new StateApplicative[E.FE]
  }

  class MonadCheck[M[_]:Provider:Monad] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    import Contextual._
    implicit val E = Provider[M].apply[Int]

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]
  }

  class MonadErrorCheck[M[_]:Provider:Sync] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    import Contextual._
    implicit val E = Provider[M].apply[Int]

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE, E]
  }

//  class ConcurrentCheck[M[_]:Provider:Concurrent] {
//
//    val monad = implicitly[Monad[M]]
//
//    val liftIO = implicitly[LiftIO[M]]
//
//    val concurrent = implicitly[Concurrent[M]]
//
//    import Contextual._
//
//    implicit val E = implicitly[Provider[M]].apply[Int]
//
//    val stateMonad = new StateMonad[E.FE]
//
//    val stateLiftIO = new StateLiftIO[E.FE]
//
//    val stateConcurrent = new StateConcurrent[E.FE]
//  }
//
//  class AsyncCheck[M[_]:Provider:Async] {
//
//    import Contextual._
//
//    implicit val E = implicitly[Provider[M]].apply[Int]
//
//    val stateMonad = new StateMonad[E.FE]
//
//    val stateLiftIO = new StateLiftIO[E.FE]
//
//  }
//
//  class LiftIOCheck[M[_]:Provider:LiftIO] {
//
//    import Contextual._
//
//    implicit val E = implicitly[Provider[M]].apply[Int]
//
//    val stateLiftIO = new StateLiftIO[E.FE]
//
//  }
//
//  class MonadCheck[M[_]:Provider:Monad] {
//
//    import Contextual._
//
//    implicit val E = implicitly[Provider[M]].apply[Int]
//
//    val stateMonad = new StateMonad[E.FE]
//
//  }
}
