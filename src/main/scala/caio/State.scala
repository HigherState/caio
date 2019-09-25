package caio

import cats.Monoid

import scala.reflect.ClassTag

case class State(content:Map[ClassTag[_], Any], eventLog:EventLog) {
  def +(c:State):State =
    State.monoid.combine(this, c)

  def +(c:Context):State =
    State(content ++ c.content, eventLog)
}

object State {

  val empty: State = State(Map.empty, Vector.empty)

  def apply(context:Context):State =
    State(context.content, Vector.empty)

  def apply(log:EventLog):State =
    State(Map.empty, log)

  def getLog(context:State): EventLog =
    context.eventLog

  def modifyLog(context:State, f:EventLog => EventLog):State =
    context.copy(eventLog = f(context.eventLog))

  implicit val monoid:Monoid[State] =
    new Monoid[State]{
      val empty: State = State.empty

      def combine(x: State, y: State): State = {
        if (x == empty) y
        else if (y == empty) x
        else {
          val lx = getLog(x)
          val ly = getLog(y)
          State(x.content ++ y.content ,lx ++ ly)
        }
      }
    }
}
