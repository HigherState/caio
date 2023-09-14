package caio.mtl

import cats.mtl.{Censor, Listen, Tell}

class ProviderWriterTests {
  type InvariantAskInt[F[_]] = InvariantAsk[F, Int]

  class AskTell[M[_]: InvariantAskInt, L](implicit T: Tell[M, L]) {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskListen[M[_]: InvariantAskInt, L](implicit L: Listen[M, L]) {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class AskCensor[M[_]: InvariantAskInt, L](implicit C: Censor[M, L]) {
    def run: M[Int] = InvariantAsk[M, Int].ask
  }

  class CensorWriter[M[_]: Provider, L](implicit C: Censor[M, L]) {

    val tell: Tell[M,L] = implicitly[Tell[M, L]]

    val listen: Listen[M,L] = implicitly[Listen[M, L]]

    val censor: Censor[M,L] = implicitly[Censor[M, L]]

    val E: Extends[M,Unit,Int] = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

    val askCensor = new AskCensor[E.FE, L]
  }

  class ListenerWriter[M[_]: Provider, L](implicit L: Listen[M, L]) {

    val E: Extends[M,Unit,Int] = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

  }

  class TellWriter[M[_]: Provider, L](implicit T: Tell[M, L]) {

    val E: Extends[M,Unit,Int] = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]
  }
}
