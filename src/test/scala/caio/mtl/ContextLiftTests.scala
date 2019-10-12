package caio.mtl

import cats.mtl.{ApplicativeAsk, ApplicativeCensor, FunctorListen, FunctorTell}

class ContextLiftTests {

  type ProvidesWithWriter[F[_], L] = Provider[F] with ContextWriterTransformers[F, L]

  class AskTell[M[_]:ApplicativeAsk[*[_], Int]:FunctorTell[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskListen[M[_]:ApplicativeAsk[*[_], Int]:FunctorListen[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

  class AskCensor[M[_]:ApplicativeAsk[*[_], Int]:ApplicativeCensor[*[_], L], L] {
    def run:M[Int] = ApplicativeAsk[M,Int].ask
  }

//  class AllWriter[M[_]:ProvidesWithWriter[*[_], L]:ApplicativeCensor[*[_], L], L] {
//    import Contextual._
//
//    implicit val E = implicitly[ProvidesWithWriter[M, L]].apply[Int]
//
//    val tell = new AskTell[E.FE, L]
//
//    val listen = new AskListen[E.FE, L]
//
//    val censor = new AskCensor[E.FE, L]
//  }
}
