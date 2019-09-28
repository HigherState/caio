package caio

import scala.reflect.ClassTag

case class ContextNotFoundException[T](tag:ClassTag[T]) extends Exception {
  def message:String = "Class not found: " + tag.runtimeClass.getSimpleName
}
