import caio.std.{CaioFailuresAsThrowable,Par}
import cats.data.NonEmptyList

package object caio {

  type ParCaio[C, V, L, +A] = Par.Type[C, V, L, A]

  type <~>[F[_], G[_]] = BijectionK[F, G]

  type ErrorOrFailure[V] = Throwable Either NonEmptyList[V]

  implicit class ErrorOps[V](val eof:ErrorOrFailure[V]) extends AnyVal {
    def toThrowable:Throwable =
      eof.fold[Throwable](identity, CaioFailuresAsThrowable.apply)
  }

}
