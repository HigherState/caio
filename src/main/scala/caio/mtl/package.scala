package caio

package object mtl {

  type WithContext[FO[_], F[_], L, E, V, Init, Additional] =
    ContextApplicator[F, L, E, V, Init, Additional]{ type FC[A] = FO[A] }

  type Provided[MP[_], M[_], E] =
    Provides[M, E]{ type FE[A] = MP[A]}

  type Extended[FP[_], F[_], E1, E2] =
    Extends[F, E1, E2]{ type FE[A] = FP[A]}

  type Asked[FP[_], F[_], E] =
    Askable[F, E]{ type FE[A] = FP[A]}
}
