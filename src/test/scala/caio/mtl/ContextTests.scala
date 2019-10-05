package caio.mtl

import caio.Failure
import cats.MonadError
import cats.effect.LiftIO
import cats.mtl.{ApplicativeAsk, MonadState}

class ContextTests {

  import caio.Event._

  //Creating internal
  class AskInt[M[_]:ApplicativeAsk[*[_], Int]:LiftIO] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }


  class AddAskContext[M[_]:LiftIO](implicit C:Context[M, Unit, EventLog, Throwable, Failure]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, Failure, Int] = C.apply[Int]
    val service = new AskInt[e.FC]

    def run:M[Int] = e.apply(3)(service.run)
  }

  class ComplexStateString[M[_]:MonadState[*[_], String]:MonadError[*[_], Throwable]:LiftIO] {
    def run:M[String] = MonadState[M,String].get
  }

  class AddStateContext[M[_]:MonadError[*[_], Throwable]:LiftIO](implicit C:Context[M, Unit, EventLog, Throwable, Failure]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, Failure, String] = C.apply[String]
    val service = new ComplexStateString[e.FC]

    def run:M[String] = e.apply("Value")(service.run)
  }

  class Dependency[M[_]:MonadError[*[_], Throwable]:LiftIO, MC[_]:WithContext[*[_], M, EventLog, Throwable, Failure, Int]]
    (a:AskInt[MC]){

    val e = implicitly[WithContext[MC, M, EventLog, Throwable, Failure, Int]]

    def run:M[Int] = e.apply(3)(a.run)
  }

}
