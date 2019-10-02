package caio.mtl

import cats.MonadError
import cats.effect.LiftIO
import cats.mtl.{ApplicativeAsk, MonadState}

class ContextTests {

  //Creating internal
  class AskInt[M[_]:ApplicativeAsk[*[_], Int]:LiftIO] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }


  class AddAskContext[M[_]](implicit C:Context[M, EventLog, Throwable]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, Int] = C.apply[Int]
    val service = new AskInt[e.FC]

    def run:M[Int] = e.apply(3)(service.run)
  }

  class ComplexStateString[M[_]:MonadState[*[_], String]:MonadError[*[_], Throwable]:LiftIO] {
    def run:M[String] = MonadState[M,String].get
  }

  class AddStateContext[M[_]:MonadError[*[_], Throwable]:LiftIO](implicit C:Context[M, EventLog, Throwable]) {
    import EnvironmentLift._

    implicit val e: EnvironmentContext[M, EventLog, Throwable, String] = C.apply[String]
    val service = new ComplexStateString[e.FC]

    def run:M[String] = e.apply("Value")(service.run)
  }

  class Dependency[M[_]:MonadError[*[_], Throwable]:LiftIO, MC[_]:WithContext[*[_], M, EventLog, Throwable, Int]]
    (a:AskInt[MC]){

    val e = implicitly[WithContext[MC, M, EventLog, Throwable, Int]]

    def run:M[Int] = e.apply(3)(a.run)
  }

}
