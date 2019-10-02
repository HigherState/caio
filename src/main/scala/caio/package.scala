import cats.data.NonEmptyList

package object caio {

  type ErrorOrFailure[V] = Throwable Either NonEmptyList[V]
}
