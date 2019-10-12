package caio.mtl

import cats.mtl.{ApplicativeAsk, ApplicativeCensor, FunctorListen, FunctorTell}

class ProviderWriterTests {

  class AskTell[M[_]:ApplicativeAsk[*[_], Int]:FunctorTell[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskListen[M[_]:ApplicativeAsk[*[_], Int]:FunctorListen[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskCensor[M[_]:ApplicativeAsk[*[_], Int]:ApplicativeCensor[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class CensorWriter[M[_]:ProviderWriter[*[_], L]:ApplicativeCensor[*[_], L], L] {

    val tell = implicitly[FunctorTell[M, L]]

    val listen = implicitly[FunctorListen[M, L]]

    val censor = implicitly[ApplicativeCensor[M, L]]

    import Contextual._

    implicit val E = implicitly[ProviderWriter[M, L]].apply[Int]

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

    val askCensor = new AskCensor[E.FE, L]
  }

  class ListenerWriter[M[_]:ProviderWriter[*[_], L]:FunctorListen[*[_], L], L] {
    import Contextual._

    implicit val E = implicitly[ProviderWriter[M, L]].apply[Int]

    val askTell = new AskTell[E.FE, L]

    val askListen = new AskListen[E.FE, L]

  }

  class TellWriter[M[_]:ProviderWriter[*[_], L]:FunctorTell[*[_], L], L] {
    import Contextual._

    implicit val E = implicitly[ProviderWriter[M, L]].apply[Int]

    val askTell = new AskTell[E.FE, L]


  }
}
