package caio

import scala.reflect.ClassTag

class Arguments private(val content:Map[ClassTag[_], Any]) extends AnyVal {
  def +[A](a:A)(implicit C:ClassTag[A]):Arguments =
    new Arguments(content + (C -> a))

  def get[A](implicit C:ClassTag[A]):Option[A] =
    content.get(C).map(_.asInstanceOf[A])

  def applyStore(s:Store):Arguments =
    new Arguments(content ++ s.content)

  override def toString: String =
    content.toString
}

object Arguments {
  val empty:Arguments =
    new Arguments(Map.empty)

  def apply[S](s:S)(implicit T:ClassTag[S]):Arguments =
    new Arguments(Map(T -> s))
}
