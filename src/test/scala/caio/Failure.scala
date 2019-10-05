package caio

import cats.data.NonEmptyList

case class Failure(value:String)

object Failure {
  type EoF = ErrorOrFailure[Failure]
  type Failures = NonEmptyList[Failure]

  val failure1 = Failure("this is a failure 1")
  val failure2 = Failure("this is a failure 2")

  val failures1 = NonEmptyList(failure1, Nil)
  val failures2 = NonEmptyList(failure2, Nil)
}
