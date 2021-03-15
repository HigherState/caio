package caio.mtl

import cats.mtl.{Censor, Listen, Tell}

class ProviderWriterTests {

  class AskTell[M[_]: InvariantAsk[*[_], Int]: Tell[*[_], L], L] {
    def run: M[Int] = InvariantAsk[M,Int].ask
  }

  class AskListen[M[_]: InvariantAsk[*[_], Int]: Listen[*[_], L], L] {
    def run: M[Int] = InvariantAsk[M,Int].ask
  }

  class AskCensor[M[_]: InvariantAsk[*[_], Int]: Censor[*[_], L], L] {
    def run: M[Int] = InvariantAsk[M,Int].ask
  }

  class CensorWriter[M[_]: Provider: Censor[*[_], L], L] {

    val tell = implicitly[Tell[M, L]]

    val listen = implicitly[Listen[M, L]]

    val censor = implicitly[Censor[M, L]]

    val E = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

    val askCensor = new AskCensor[E.FE, L]
  }

  class ListenerWriter[M[_]: Provider: Listen[*[_], L], L] {

    val E = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

  }

  class TellWriter[M[_]: Provider: Tell[*[_], L], L] {

    val E = implicitly[Provider[M]].apply[Int]
    import E._

    val askTell = new AskTell[E.FE, L]
  }
}
