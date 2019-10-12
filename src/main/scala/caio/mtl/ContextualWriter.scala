package caio.mtl

import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextualWriter {

  implicit def toWriterTransformAux[F[_], L](implicit E:ContextWriterTransformers[F, L]): WriterTransform[E.FE, F, L] =
    E.asInstanceOf[WriterTransform[E.FE, F, L]]

  implicit def functorTellFE[F[_], L](implicit F:ToTell[F, L]):FunctorTell[F, L] =
    F.apply()

  implicit def functorListenFE[F[_], L](implicit F:ToListen[F, L]):FunctorListen[F, L] =
    F.apply()

  implicit def applicativeCensorFE[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L] =
    P.transformApplicativeCensor(F)


  implicit def tellToTell[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:FunctorTell[F, L]):ToTell[FE, L] =
    TellToTell(P, F)

  implicit def listenToTell[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:FunctorListen[F, L]):ToTell[FE, L] =
    ListenToTell(P, F)

  implicit def censorToTell[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ToTell[FE, L] =
    CensorToTell(P, F)


  implicit def listenToListen[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:FunctorListen[F, L]):ToListen[FE, L] =
    ListenToListen(P, F)

  implicit def censorToListen[F[_], FE[_], L](implicit P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]):ToListen[FE, L] =
    CensorToListen(P, F)
}

sealed trait ToListen[F[_], L] {
  def apply():FunctorListen[F, L]
}
case class CensorToListen[F[_], FE[_], L](P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]) extends ToListen[FE, L] {
  def apply():FunctorListen[FE, L] =
    P.transformFunctorListen(F)
}
case class ListenToListen[F[_], FE[_], L, C](P:WriterTransform[FE, F, L], F:FunctorListen[F, L]) extends ToListen[FE, L] {
  def apply():FunctorListen[FE, L] =
    P.transformFunctorListen(F)
}

sealed trait ToTell[F[_], L] {
  def apply():FunctorTell[F, L]
}
case class CensorToTell[F[_], FE[_], L](P:WriterTransform[FE, F, L], F:ApplicativeCensor[F, L]) extends ToTell[FE, L] {
  def apply():FunctorTell[FE, L] =
    P.transformFunctorTell(F)
}
case class ListenToTell[F[_], FE[_], L](P:WriterTransform[FE, F, L], F:FunctorListen[F, L]) extends ToTell[FE, L] {
  def apply():FunctorTell[FE, L] =
    P.transformFunctorTell(F)
}
case class TellToTell[F[_], FE[_], L](P:WriterTransform[FE, F, L], F:FunctorTell[F, L]) extends ToTell[FE, L] {
  def apply():FunctorTell[FE, L] =
    P.transformFunctorTell(F)
}