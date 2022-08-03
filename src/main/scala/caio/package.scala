import caio.std.Par

package object caio {
  type ParCaio[C, L, +A] = Par.Type[C, L, A]

  type <~>[F[_], G[_]] = BijectionK[F, G]
}
