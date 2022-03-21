import cats.effect.{Fiber, Outcome}

package object caio {
  type <~>[F[_], G[_]] = BijectionK[F, G]

  type OutcomeCaio[C, L, A] = Outcome[Caio[C, L, *], Throwable, A]

  type FiberCaio[C, L, A] = Fiber[Caio[C, L, *], Throwable, A]
}
