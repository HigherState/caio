package caio

package object mtl {

  type Provided[MP[_], M[_], E] =
    Provides[M, E]{ type FE[A] = MP[A]}

  type ProvidedWriter[MP[_], M[_], L, E] =
    ProvidesWriter[M, L, E]{ type FE[A] = MP[A]}

  type ExtendedWriter[FP[_], F[_], L, E1, E2] =
    ExtendsWriter[F, L, E1, E2]{ type FE[A] = FP[A]}

  type Asked[FP[_], F[_], E] =
    Askable[F, E]{ type FE[A] = FP[A]}


  type WriterTransform[FP[_], F[_], L] =
    ContextWriterTransformers[F, L]{ type FE[A] = FP[A]}

  type Transform[FP[_], F[_]] =
    ContextTransformers[F]{ type FE[A] = FP[A]}
}
