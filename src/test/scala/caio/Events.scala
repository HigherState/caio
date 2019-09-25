package caio

case class TestEvent(id:Int) extends Event

object Events {
  val one = TestEvent(1)
  val two = TestEvent(2)
  val three = TestEvent(3)
  val four = TestEvent(4)
}
