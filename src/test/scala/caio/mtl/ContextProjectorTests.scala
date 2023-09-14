package caio.mtl

import cats.mtl.Stateful
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers

class ContextProjectorTests extends AsyncFunSpec with Matchers {
  type InvariantAskInt[F[_]]       = InvariantAsk[F, Int]
  type InvariantAskString[F[_]]    = InvariantAsk[F, String]
  type InvariantAskIntString[F[_]] = InvariantAsk[F, (Int, String)]


  type StatefulInt[F[_]]       = Stateful[F, Int]
  type StatefulString[F[_]]    = Stateful[F, String]
  type StatefulIntString[F[_]] = Stateful[F, (Int, String)]

  class AskInt[M[_]: InvariantAskInt] {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskString[M[_]: InvariantAskString] {
    def run: M[String] = InvariantAsk[M, String].ask
  }

  class AskIntString[M[_]: InvariantAskIntString] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]

    val as = new AskString[M]
  }

  class AskIntIgnore[M[_]: InvariantAskInt] {
    import caio.mtl.ContextProjector._

    val ai = new AskInt[M]
  }

  class MonadInt[M[_]: StatefulInt] {
    def run: M[Int] = Stateful[M, Int].get
  }

  class MonadString[M[_]: StatefulString] {
    def run: M[String] = Stateful[M, String].get
  }

  class MonadIntString[M[_]: StatefulIntString] {
    import caio.mtl.ContextProjector._
    import Stateful._

    val ai = new MonadInt[M]

    val as = new MonadString[M]
  }

  class MonadIntAskString[M[_]: StatefulIntString] {
    import caio.mtl.ContextCombinator.{combinatorAsk, stateCombinator}

    val ai = new AskInt[M]

    val as = new AskString[M]
  }

  class MonadIntAskStringF[M[_]: StatefulInt: InvariantAskString] {
    import caio.mtl.ContextCombinator.{askStateCombinator, combinatorAsk}

    val as = new AskIntString[M]
  }
}
