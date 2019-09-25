package caio

trait Failure

case class Failures(head:Failure, tail:List[Failure]) extends Throwable {
  def toList:List[Failure] = head :: tail
}

case class Throwables(head:Throwable, tail:List[Throwable]) extends Throwable {
  def toList:List[Throwable] = head :: tail
}

object Throwables {

  def append:Function[(Throwable, Throwable), Throwable] = {
    case (Throwables(h1, t1), Throwables(h2, t2)) =>
      Throwables(h1,t1 ++ (h2 :: t2))
    case (t, Throwables(h2, t2)) =>
      Throwables(t, h2 :: t2)
    case (Throwables(h1, t1), t) =>
      Throwables(h1, t1 :+ t)
    case (t1, t2) =>
      Throwables(t1, List(t2))
  }
}

object Failures {
  def apply(head:Failure, tail:Failure*):Failures =
    Failures(head, tail.toList)
}
