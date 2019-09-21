package caio

trait Failure

case class Failures(head:Failure, tail:List[Failure]) extends Throwable {
  def toList:List[Failure] = head :: tail
}
