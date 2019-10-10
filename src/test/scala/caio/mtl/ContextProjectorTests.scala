package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import org.scalatest.{AsyncFunSpec, Matchers}

class ContextProjectorTests extends AsyncFunSpec with Matchers{

  class AskInt[M[_]:ApplicativeAsk[*[_], Int]] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskString[M[_]:ApplicativeAsk[*[_], String]] {
    def run:M[String] = ApplicativeAsk[M, String].ask
  }

  class AskIntString[M[_]:ApplicativeAsk[*[_], (Int, String)]] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]

    val as = new AskString[M]
  }

  class AskIntIgnore[M[_]:ApplicativeAsk[*[_], Int]] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]
  }

  class MonadInt[M[_]:MonadState[*[_], Int]] {
    def run:M[Int] = MonadState[M,Int].get
  }

  class MonadString[M[_]:MonadState[*[_], String]] {
    def run:M[String] = MonadState[M, String].get
  }

  class MonadIntString[M[_]:MonadState[*[_], (Int, String)]] {

    import caio.mtl.ContextProjector._

    val ai = new MonadInt[M]

    val as = new MonadString[M]
  }


  class MonadIntAskString[M[_]:MonadState[*[_], (Int, String)]] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]

    val as = new AskString[M]
  }
//unresolved implicit diverging
//
//  class MonadIntAskStringF[M[_]:MonadState[*[_], Int]:ApplicativeAsk[*[_], String]] {
//    import caio.mtl.ArgsProjector._
//
//    val as = new AskIntString[M]
//  }
}
