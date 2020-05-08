package caio.std

import cats.data.NonEmptyList

case class CaioFailuresAsThrowable[V](failure:NonEmptyList[V]) extends Throwable {

  override def getStackTrace: Array[StackTraceElement] = Array.empty
}