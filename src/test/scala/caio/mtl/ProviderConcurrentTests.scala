package caio.mtl

import cats.{Applicative, Functor, Monad, MonadError}
import cats.effect.{Async, Bracket, Concurrent, LiftIO, Sync}
import cats.mtl.{Censor, Listen, Stateful, Tell}

class ProviderConcurrentTests {

  class StateFunctor[M[_]: Stateful[*[_], Int]: Functor] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateApplicative[M[_]: Stateful[*[_], Int]: Applicative] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateMonad[M[_]: Stateful[*[_], Int]: Monad] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class AskMonadError[M[_]: InvariantAsk[*[_], Int]: MonadError[*[_], Throwable]] {
    def run: M[Int] = InvariantAsk[M, Int].ask

    def fail: M[Unit] =
      MonadError[M, Throwable].raiseError(new Exception("Test"))
  }

  class AskBracket[M[_]: InvariantAsk[*[_], Int]: Bracket[*[_], Throwable]] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class StateSync[M[_]: Stateful[*[_], Int]: Sync] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateAsync[M[_]: Stateful[*[_], Int]: Async] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class StateConcurrent[M[_]: Stateful[*[_], Int]: Concurrent] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class AskLiftIO[M[_]: InvariantAsk[*[_], Int]: LiftIO] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskFail[M[_]: InvariantAsk[*[_], Int]: ApplicativeFail[*[_], V], V] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class FunctorCheck[M[_]: Provider: Monad] {
    val functor = implicitly[Functor[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class FunctorCheck2[M[_]: Provider: Sync] {
    val functor = implicitly[Functor[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class FunctorCheck3[M[_]: Provider: Concurrent] {
    val functor = implicitly[Functor[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateFunctor = new StateFunctor[E.FE]
  }

  class MonadCheck[M[_]: Provider: Sync] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]
  }

  class MonadErrorCheck[M[_]: Provider: Sync] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]
  }

  class BracketCheck[M[_]: Provider: Sync] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val bracket = implicitly[Bracket[M, Throwable]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askBracket = new AskBracket[E.FE]
  }

  class SyncCheck[M[_]: Provider: Sync] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val bracket = implicitly[Bracket[M, Throwable]]

    val sync = implicitly[Sync[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askBracket = new AskBracket[E.FE]

    val syncBracket = new StateSync[E.FE]
  }

  class LiftIOCheck[M[_]: Provider: LiftIO] {

    val liftIO = implicitly[LiftIO[M]]

    val E = Provider[M].apply[Int]
    import E._

    val askLiftIO = new AskLiftIO[E.FE]
  }

  class AsyncCheck[M[_]: Provider: Concurrent] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val bracket = implicitly[Bracket[M, Throwable]]

    val sync = implicitly[Sync[M]]

    val async = implicitly[Async[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askBracket = new AskBracket[E.FE]

    val stateSync = new StateSync[E.FE]

    val stateAsync = new StateAsync[E.FE]

    val askLiftIO = new AskLiftIO[E.FE]
  }

  class ConcurrentCheck[M[_]: Provider: Concurrent] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val bracket = implicitly[Bracket[M, Throwable]]

    val sync = implicitly[Sync[M]]

    val async = implicitly[Async[M]]

    val concurrent = implicitly[Concurrent[M]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askBracket = new AskBracket[E.FE]

    val stateSync = new StateSync[E.FE]

    val stateAsync = new StateAsync[E.FE]

    val askLiftIO = new AskLiftIO[E.FE]

    val stateConcurrent = new StateConcurrent[E.FE]
  }

  class TotalCheck[M[_]: Provider: Concurrent: ApplicativeFail[*[_], V]: Censor[*[_], L], V, L] {

    val functor = implicitly[Functor[M]]

    val applicative = implicitly[Applicative[M]]

    val monad = implicitly[Monad[M]]

    val monadError = implicitly[MonadError[M, Throwable]]

    val bracket = implicitly[Bracket[M, Throwable]]

    val sync = implicitly[Sync[M]]

    val async = implicitly[Async[M]]

    val concurrent = implicitly[Concurrent[M]]

    val liftIO = implicitly[LiftIO[M]]

    val fail = implicitly[ApplicativeFail[M, V]]

    val tell = implicitly[Tell[M, L]]

    val listen = implicitly[Listen[M, L]]

    val censor = implicitly[Censor[M, L]]

    val E = Provider[M].apply[Int]
    import E._

    val stateApplicative = new StateApplicative[E.FE]

    val stateFunctor = new StateFunctor[E.FE]

    val stateMonad = new StateMonad[E.FE]

    val askMonadError = new AskMonadError[E.FE]

    val askBracket = new AskBracket[E.FE]

    val stateSync = new StateSync[E.FE]

    val stateAsync = new StateAsync[E.FE]

    val askLiftIO = new AskLiftIO[E.FE]

    val stateConcurrent = new StateConcurrent[E.FE]

    val askFail = new AskFail[E.FE, V]

    val T       = new ProviderWriterTests
    val askTell = new T.AskTell[E.FE, L]

    val askListen = new T.AskListen[E.FE, L]

    val askCensor = new T.AskCensor[E.FE, L]
  }
}
