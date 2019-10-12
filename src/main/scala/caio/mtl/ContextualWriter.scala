package caio.mtl

import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextualWriter {

  implicit def functorTellFE[F[_], L](implicit F:ToTell[F, L]):FunctorTell[F, L] =
    null

  implicit def tellToTell[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:FunctorTell[F, L]):ToTell[FE, L] =
    TellToTell(P, F)

  implicit def listenToTell[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:FunctorListen[F, L]):ToTell[FE, L] =
    ListenToTell(P, F)

  implicit def censorToTell[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ToTell[FE, L] =
    CensorToTell(P, F)


  implicit def functorListenFE[F[_], L ](implicit F:ToListen[F, L]):FunctorListen[F, L] =
    null

  implicit def listenToListen[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:FunctorListen[F, L]):ToListen[FE, L] =
    ListenToListen(P, F)

  implicit def censorToListen[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ToListen[FE, L] =
    CensorToListen(P, F)



  implicit def applicativeCensorFE[F[_], FE[_], L, C](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L] =
    null
}

sealed trait ToListen[F[_], L]
case class CensorToListen[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]) extends ToListen[FE, L]
case class ListenToListen[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:FunctorListen[F, L]) extends ToListen[FE, L]

sealed trait ToTell[F[_], L]
case class CensorToTell[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]) extends ToTell[FE, L]
case class ListenToTell[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:FunctorListen[F, L]) extends ToTell[FE, L]
case class TellToTell[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:FunctorTell[F, L]) extends ToTell[FE, L]