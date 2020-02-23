package caio

package object mtl {

  type Provider[F[_]] =
    Extender[F, Unit]

  type Provided[MP[_], M[_], E] =
    Extends[M, Unit, E]{ type FE[A] = MP[A]}

  type Provides[F[_], E] =
    Extends[F, Unit, E]

  type Extended[FP[_], F[_], E1, E2] =
    Extends[F, E1, E2]{ type FE[A] = FP[A]}

  type PartialExtended[FP[_], F[_], E1, E2] =
    PartialExtends[F, E1, E2]{ type FE[A] = FP[A]}

  type Transform[FP[_], F[_]] =
    ContextTransformers[F]{ type FE[A] = FP[A]}
}
