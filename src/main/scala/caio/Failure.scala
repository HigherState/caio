package caio

trait Failure

case class Failures(head:Failure, tail:List[Failure]) extends Throwable {
  def toList:List[Failure] = head :: tail
}

case class Throwables(head:Throwable, tail:List[Throwable]) extends Throwable {
  def toList:List[Throwable] = head :: tail
}

object Throwables {
  def apply(head:Throwable, tail:Throwable*):Throwables =
    Throwables(head, tail.toList)
}

object Failures {
  def apply(head:Failure, tail:Failure*):Failures =
    Failures(head, tail.toList)
}
