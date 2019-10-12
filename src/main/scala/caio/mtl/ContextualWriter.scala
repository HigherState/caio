package caio.mtl

import cats.mtl.{ApplicativeCensor, FunctorListen, FunctorTell}

trait ContextualWriter {

  implicit def functorTellFE[F[_], L](implicit F:ToTell[F, L]):FunctorTell[F, L] =
    F.f

  implicit def functorListenFE[F[_], L](implicit F:ToListen[F, L]):FunctorListen[F, L] =
    F.f

  implicit def applicativeCensorFE[F[_], FE[_], L](implicit P:Transform[FE, F], F:ApplicativeCensor[F, L]):ApplicativeCensor[FE, L] =
    P.transformApplicativeCensor(F)


  implicit def tellToTell[F[_], FE[_], L](implicit P:Transform[FE, F], F:FunctorTell[F, L]):ToTell[FE, L] =
    ToTell(P.transformFunctorTell(F))

  implicit def listenToTell[F[_], FE[_], L](implicit P:Transform[FE, F], F:FunctorListen[F, L]):ToTell[FE, L] =
    ToTell(P.transformFunctorTell(F))

  implicit def censorToTell[F[_], FE[_], L](implicit P:Transform[FE, F], F:ApplicativeCensor[F, L]):ToTell[FE, L] =
    ToTell(P.transformFunctorTell(F))

  implicit def listenToListen[F[_], FE[_], L](implicit P:Transform[FE, F], F:FunctorListen[F, L]):ToListen[FE, L] =
    ToListen(P.transformFunctorListen(F))

  implicit def censorToListen[F[_], FE[_], L](implicit P:Transform[FE, F], F:ApplicativeCensor[F, L]):ToListen[FE, L] =
    ToListen(P.transformFunctorListen(F))
}

case class ToListen[F[_], L](f:FunctorListen[F, L]) extends AnyVal
case class ToTell[F[_], L](f:FunctorTell[F, L]) extends AnyVal
