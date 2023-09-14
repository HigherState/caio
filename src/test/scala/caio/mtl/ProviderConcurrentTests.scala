package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError, MonadThrow}
import cats.effect.{Async, Concurrent, LiftIO, MonadCancel, MonadCancelThrow, Sync}
import cats.mtl.{Censor, Listen, Stateful, Tell}

class ProviderConcurrentTests {
  type InvariantAskInt[F[_]] = InvariantAsk[F, Int]
  type StatefulInt[F[_]]     = Stateful[F, Int]

  class StateFunctor[M[_]: StatefulInt: Functor] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateApplicative[M[_]: StatefulInt: Applicative] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateMonad[M[_]: StatefulInt: Monad] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class AskMonadError[M[_]: InvariantAskInt: MonadThrow] {
    def run: M[Int] = InvariantAsk[M, Int].ask

    def fail: M[Unit] =
      MonadError[M, Throwable].raiseError(new Exception("Test"))
  }

  class AskMonadCancel[M[_]: InvariantAskInt: MonadCancelThrow] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class StateSync[M[_]: StatefulInt: Sync] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateAsync[M[_]: StatefulInt: Async] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateConcurrent[M[_]: StatefulInt: Concurrent] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class AskLiftIO[M[_]: InvariantAskInt: LiftIO] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class FunctorCheck[M[_]: Provider: Monad] {
    val functor: Functor[M] = implicitly[Functor[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class FunctorCheck2[M[_]: Provider: Sync] {
    val functor: Functor[M] = implicitly[Functor[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class FunctorCheck3[M[_]: Provider: Concurrent] {
    val functor: Functor[M] = implicitly[Functor[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class MonadCheck[M[_]: Provider: Sync] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]
  }

  class MonadErrorCheck[M[_]: Provider: Sync] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]
  }

  class BracketCheck[M[_]: Provider: Sync] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val monadCancel: MonadCancel[M,Throwable] = implicitly[MonadCancel[M, Throwable]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askMonadCancel = new AskMonadCancel[E.FE]
  }

  class SyncCheck[M[_]: Provider: Sync] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val monadCancel: MonadCancel[M,Throwable] = implicitly[MonadCancel[M, Throwable]]

    val sync: Sync[M] = implicitly[Sync[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askMonadCancel = new AskMonadCancel[E.FE]

    val syncBracket = new StateSync[E.FE]
  }

  class LiftIOCheck[M[_]: Provider: LiftIO] {

    val liftIO: LiftIO[M] = implicitly[LiftIO[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val askLiftIO = new AskLiftIO[E.FE]
  }

  class AsyncCheck[M[_]: Provider: Concurrent] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val monadCancel: MonadCancel[M,Throwable] = implicitly[MonadCancel[M, Throwable]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askMonadCancel = new AskMonadCancel[E.FE]
  }

  class ConcurrentCheck[M[_]: Provider: Concurrent] {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val monadCancel: MonadCancel[M,Throwable] = implicitly[MonadCancel[M, Throwable]]

    val concurrent: Concurrent[M] = implicitly[Concurrent[M]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askMonadCancel = new AskMonadCancel[E.FE]

    val stateConcurrent = new StateConcurrent[E.FE]
  }

  class TotalCheck[M[_]: Provider: Async, L](implicit C: Censor[M, L]) {

    val functor: Functor[M] = implicitly[Functor[M]]

    val applicative: Applicative[M] = implicitly[Applicative[M]]

    val monad: Monad[M] = implicitly[Monad[M]]

    val monadError: MonadError[M,Throwable] = implicitly[MonadError[M, Throwable]]

    val monadCancel: MonadCancel[M,Throwable] = implicitly[MonadCancel[M, Throwable]]

    val sync: Sync[M] = implicitly[Sync[M]]

    val async: Async[M] = implicitly[Async[M]]

    val concurrent: Concurrent[M] = implicitly[Concurrent[M]]

    val tell: Tell[M,L] = implicitly[Tell[M, L]]

    val listen: Listen[M,L] = implicitly[Listen[M, L]]

    val censor: Censor[M,L] = implicitly[Censor[M, L]]

    val E: Extends[M,Unit,Int] = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askMonadCancel = new AskMonadCancel[E.FE]

    val stateSync = new StateSync[E.FE]

    val stateAsync = new StateAsync[E.FE]

    val stateConcurrent = new StateConcurrent[E.FE]

    val T       = new ProviderWriterTests
    val askTell = new T.AskTell[E.FE, L]

    val askListen = new T.AskListen[E.FE, L]

    val askCensor = new T.AskCensor[E.FE, L]
  }
}
