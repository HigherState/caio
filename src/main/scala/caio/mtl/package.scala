package caio

package object mtl {

  type WithContext[FO[_], F[_], L, E, V, C] =
    EnvironmentContext[F, L, E, V, C]{ type FC[A] = FO[A] }
}
