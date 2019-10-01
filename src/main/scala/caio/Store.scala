package caio

import cats.Monoid

/**
 * Holds any State change and log information for the Caio Monad
 * @tparam C State/environment arguments
 * @tparam L Log
 */
private[caio] sealed trait Store[C, L] {
  def +(s:Store[C, L]):Store[C, L]

  def getOrElse(c:C):C

  def l:L

  def map(f:L => L):Store[C, L]
}

private[caio] case class LogStore[C, L:Monoid](l:L)
  extends Store[C, L] {

  def +(s:Store[C, L]):Store[C, L] =
    s match {
      case ContentStore(c, l) =>
        ContentStore(c, implicitly[Monoid[L]].combine(l, l))
      case LogStore(l) =>
        LogStore(implicitly[Monoid[L]].combine(l, l))
    }

  def getOrElse(c:C):C =
    c

  def map(f:L => L):Store[C, L] =
    LogStore(f(l))
}

private[caio] case class ContentStore[C, L:Monoid](c:C, l:L)
  extends Store[C, L] {

  def +(s:Store[C, L]):Store[C, L] =
    s match {
      case ContentStore(c, l) =>
        ContentStore(c, implicitly[Monoid[L]].combine(l, l))
      case LogStore(l) =>
        ContentStore(c, implicitly[Monoid[L]].combine(l, l))
    }

  def getOrElse(c:C):C =
    c

  def map(f:L => L):Store[C, L] =
    ContentStore(c, f(l))
}

object Store {
  def empty[C, L:Monoid]:Store[C, L] =
    LogStore(implicitly[Monoid[L]].empty)
}
