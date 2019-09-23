package caio

import cats.Monoid

import scala.reflect.ClassTag

case class Context(content:Map[ClassTag[_], Any], eventLog:EventLog) {
  def +(c:Context):Context =
    Context.monoid.combine(this, c)
}

object Context {

  val empty: Context = Context(Map.empty, Vector.empty)

  def getLog(context:Context): EventLog =
    context.eventLog

  def modifyLog(context:Context, f:EventLog => EventLog):Context =
    context.copy(eventLog = f(context.eventLog))

  implicit val monoid:Monoid[Context] =
    new Monoid[Context]{
      val empty: Context = Context.empty

      def combine(x: Context, y: Context): Context = {
        if (x == empty) y
        else if (y == empty) x
        else {
          val lx = getLog(x)
          val ly = getLog(y)
          Context(x.content ++ y.content ,lx ++ ly)
        }
      }
    }
}
