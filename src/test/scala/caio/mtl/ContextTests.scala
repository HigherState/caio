package caio.mtl

import caio.EventLog
import cats.MonadError
import cats.effect.Sync
import cats.mtl.{ApplicativeAsk, MonadState}

class ContextTests {

  //Creating internal
  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }


  class AddAskContext[M[_]](implicit C:Context[M, EventLog, Throwable]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, Int] = C.apply[Int]
    val service = new AskInt[e.FC]

    def run:M[Int] = e.apply(3)(service.run)
  }

  class ComplexStateString[M[_]:MonadState[*[_], String]:Sync:MonadError[*[_], Throwable]] {
    def run:M[String] = MonadState[M,String].get
  }

  class AddStateContext[M[_]:Sync:MonadError[*[_], Throwable]](implicit C:Context[M, EventLog, Throwable]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, String] = C.apply[String]
    val service = new ComplexStateString[e.FC]

    def run:M[String] = e.apply("Value")(service.run)
  }

}