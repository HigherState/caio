import caio.std.CaioFailuresAsThrowable
import cats.data.NonEmptyList

package object caio {

  type <~>[F[_], G[_]] = BijectionK[F, G]

  type ErrorOrFailure[V] = Throwable Either NonEmptyList[V]

  implicit class ErrorOps[V](val eof:ErrorOrFailure[V]) extends AnyVal {
    def toThrowable:Throwable =
      eof.fold[Throwable](identity, CaioFailuresAsThrowable.apply)
  }

}
