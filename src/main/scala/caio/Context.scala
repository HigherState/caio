package caio

import scala.reflect.ClassTag

case class Context(content:Map[ClassTag[_], Any]) extends AnyVal {
  def +[A](a:A)(implicit C:ClassTag[A]) = Context(content + (C -> a) )
}

object Context {
  val empty:Context = Context(Map.empty)
}
