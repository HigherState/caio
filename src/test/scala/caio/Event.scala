package caio

import cats.{Eq, Monoid}

trait Event

case class TestEvent(id: Int) extends Event

object Event {
  type EventLog = Vector[Event]

  implicit val EventMonoid: Monoid[EventLog] =
    new Monoid[EventLog] {
      def empty: EventLog =
        Vector.empty

      def combine(x: EventLog, y: EventLog): EventLog =
        x ++ y
    }

  implicit val EventEq: Eq[EventLog] =
    (x: EventLog, y: EventLog) => x.equals(y)

  val event1: TestEvent = TestEvent(1)
  val event2: TestEvent = TestEvent(2)
  val event3: TestEvent = TestEvent(3)
  val event4: TestEvent = TestEvent(4)
}
