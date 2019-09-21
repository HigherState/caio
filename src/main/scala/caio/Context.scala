package caio

import cats.Monoid

import scala.reflect.ClassTag

case class Context(content:Map[ClassTag[_], Any]) extends AnyVal {
  def +(c:Context):Context =
    Context.monoid.combine(this, c)
}

object Context {

  val empty: Context = Context(Map.empty)

  val eventLogTag: ClassTag[EventLog] = implicitly[ClassTag[EventLog]]

  def getLog(context:Context): Vector[Event] =
    context.content.get(eventLogTag).collect{ case v:Vector[Event]@unchecked => v}.getOrElse(Vector.empty)

  implicit val monoid:Monoid[Context] = new Monoid[Context]{
    val empty: Context = Context.empty

    def combine(x: Context, y: Context): Context = {
      if (x == empty) y
      else if (y == empty) x
      else {
        val lx = getLog(x)
        val ly = getLog(y)
        Context(x.content ++ y.content + (eventLogTag -> (lx ++ ly)))
      }
    }
  }
}
