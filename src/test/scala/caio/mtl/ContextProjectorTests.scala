package caio.mtl

import cats.mtl.Stateful
import org.scalatest.{AsyncFunSpec, Matchers}

class ContextProjectorTests extends AsyncFunSpec with Matchers {

  class AskInt[M[_]: InvariantAsk[*[_], Int]] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskString[M[_]: InvariantAsk[*[_], String]] {
    def run: M[String] = InvariantAsk[M, String].ask
  }

  class AskIntString[M[_]: InvariantAsk[*[_], (Int, String)]] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]

    val as = new AskString[M]
  }

  class AskIntIgnore[M[_]: InvariantAsk[*[_], Int]] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]
  }

  class MonadInt[M[_]: Stateful[*[_], Int]] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class MonadString[M[_]: Stateful[*[_], String]] {
    def run: M[String] = Stateful[M, String].get
  }

  class MonadIntString[M[_]: Stateful[*[_], (Int, String)]] {
    import caio.mtl.ContextProjector._
    import Stateful._

    val ai = new MonadInt[M]

    val as = new MonadString[M]
  }

  class MonadIntAskString[M[_]: Stateful[*[_], (Int, String)]] {
    import caio.mtl.ContextCombinator.{combinatorAsk, stateCombinator}

    val ai = new AskInt[M]

    val as = new AskString[M]
  }

  class MonadIntAskStringF[M[_]: Stateful[*[_], Int]: InvariantAsk[*[_], String]] {
    import caio.mtl.ContextCombinator.{askStateCombinator, combinatorAsk}

    val as = new AskIntString[M]
  }
}
