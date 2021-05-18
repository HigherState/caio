package caio

import cats.Monoid

/**
 * Holds any State change and log information for the Caio Monad
 * @tparam C State/environment arguments
 * @tparam L Log
 */
sealed private[caio] trait Store[+C, +L] {

  def combine[C1 >: C, L1 >: L](s: Store[C1, L1]): Store[C1, L1]

  def getLog[L1 >: L](implicit M: Monoid[L1]): L1

  def getContext[C1 >: C](default: C1): C1

  def map[L1 >: L](f: L => L1): Store[C, L1]
}

final private[caio] case class LogStore[C, L](log: L, monoid: Monoid[L]) extends Store[C, L] {
  def combine[C1 >: C, L1 >: L](s: Store[C1, L1]): Store[C1, L1] =
    s match {
      case ContentStore(context, log2, monoid2) =>
        ContentStore(
          context,
          monoid2.asInstanceOf[Monoid[L1]].combine(log.asInstanceOf[L1], log2),
          monoid2.asInstanceOf[Monoid[L1]]
        )
      case LogStore(log2, monoid2)              =>
        LogStore(monoid2.asInstanceOf[Monoid[L1]].combine(log.asInstanceOf[L1], log2), monoid2.asInstanceOf[Monoid[L1]])
      case EmptyStore                           =>
        this
    }

  def getLog[L1 >: L](implicit M: Monoid[L1]): L1 = log

  def getContext[C1 >: C](default: C1): C1 = default

  def map[L1 >: L](f: L => L1): Store[C, L1] =
    LogStore(f(log), monoid.asInstanceOf[Monoid[L1]])
}

final private[caio] case class ContentStore[C, L](context: C, log: L, monoid: Monoid[L]) extends Store[C, L] {
  def combine[C1 >: C, L1 >: L](s: Store[C1, L1]): Store[C1, L1] =
    s match {
      case ContentStore(context2, log2, monoid2) =>
        ContentStore(
          context2,
          monoid2.asInstanceOf[Monoid[L1]].combine(log.asInstanceOf[L1], log2),
          monoid2.asInstanceOf[Monoid[L1]]
        )
      case LogStore(log2, monoid2)               =>
        ContentStore(
          context,
          monoid2.asInstanceOf[Monoid[L1]].combine(log.asInstanceOf[L1], log2),
          monoid2.asInstanceOf[Monoid[L1]]
        )
      case EmptyStore                            =>
        this
    }

  def getLog[L1 >: L](implicit M: Monoid[L1]): L1 = log

  def getContext[C1 >: C](default: C1): C1 = context

  def map[L1 >: L](f: L => L1): Store[C, L1] =
    ContentStore(context, f(log), monoid.asInstanceOf[Monoid[L1]])
}

private[caio] object EmptyStore extends Store[Nothing, Nothing] {
  def combine[C1 >: Nothing, L1 >: Nothing](s: Store[C1, L1]): Store[C1, L1] = s

  def getLog[L1 >: Nothing](implicit M: Monoid[L1]): L1 = M.empty

  def getContext[C1 >: Nothing](default: C1): C1 = default

  def map[L1 >: Nothing](f: Nothing => L1): Store[Nothing, L1] = this
}
