package caio

package object mtl {

  type WithContext[FO[_], F[_], L, E, C] =
    EnvironmentContext[F, L, E, C]{ type FC[A] = FO[A] }
}
