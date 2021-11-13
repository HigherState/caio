import caio.std.Par

package object caio {

  type ParCaio[C, V, L, +A] = Par.Type[C, V, L, A]

  type <~>[F[_], G[_]] = BijectionK[F, G]

}
