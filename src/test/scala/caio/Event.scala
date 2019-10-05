package caio

import cats.Monoid

trait Event

case class TestEvent(id:Int) extends Event

object Event {
  type EventLog = Vector[Event]

  implicit val EventMonoid:Monoid[EventLog] =
    new Monoid[EventLog] {
      def empty: EventLog =
        Vector.empty

      def combine(x: EventLog, y: EventLog): EventLog =
        x ++ y
    }

  val event1 = TestEvent(1)
  val event2 = TestEvent(2)
  val event3 = TestEvent(3)
  val event4 = TestEvent(4)
}
