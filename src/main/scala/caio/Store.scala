package caio

import cats.Monoid

import scala.reflect.ClassTag

case class Store private(content:Map[ClassTag[_], Any], eventLog:EventLog) {
  def +(c:Store):Store =
    Store.monoid.combine(this, c)

  def +(c:Arguments):Store =
    Store(content ++ c.content, eventLog)

  def set[S](s:S)(implicit T:ClassTag[S]):Store =
    this.copy(content = content + (T -> s))
}

object Store {

  val empty: Store = Store(Map.empty, Vector.empty)

  def apply(arguments:Arguments):Store =
    Store(arguments.content, Vector.empty)

  def apply(log:EventLog):Store =
    Store(Map.empty, log)

  def getLog(store:Store): EventLog =
    store.eventLog

  def modifyLog(store:Store, f:EventLog => EventLog):Store =
    store.copy(eventLog = f(store.eventLog))

  implicit val monoid:Monoid[Store] =
    new Monoid[Store]{
      val empty: Store = Store.empty

      def combine(x: Store, y: Store): Store = {
        if (x == empty) y
        else if (y == empty) x
        else {
          val lx = getLog(x)
          val ly = getLog(y)
          Store(x.content ++ y.content ,lx ++ ly)
        }
      }
    }
}
