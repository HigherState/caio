package caio

import cats.{Eq, Monoid}
import cats.data.NonEmptyList
import cats.effect.IO

sealed trait Caio[C, V, L, A] {

}

final private[caio] case class PureCaio[C, V, L, A](a: A) extends Caio[C, V, L, A]

final private[caio] case class IOCaio[C, V, L, A](a: IO[A]) extends Caio[C, V, L, A]

final private[caio] case class LogCaio[C, V, L](l:L) extends Caio[C, V, L, Unit]

final private[caio] case class ErrorCaio[C, V, L](e:Throwable) extends Caio[C, V, L, Unit]

final private[caio] case class FailureCaio[C, V, L](head:V, tail:List[V]) extends Caio[C, V, L, Unit]

final private[caio] case class KleisliCaio[C, V, L, E, A](kleisli: C => Caio[C, V, L, A]) extends Caio[C, V, L, A]


final private[caio] case class BindCaio[C, V, L, E, A](source: Caio[C, V, L, E], f: E => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class MapCaio[C, V, L, E, A](source: Caio[C, V, L, E], f: E => A) extends Caio[C, V, L, A]

final private[caio] case class HandleErrorCaio[C, V, L, A](source: Caio[C, V, L, A], f: Throwable => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class HandleFailureCaio[C, V, L,  A](source: Caio[C, V, L, A], f: NonEmptyList[V] => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class WithLogCaio[C, V, L, E, A](source: Caio[C, V, L, E], f:(E, L) => (L, Caio[C, V, L, A])) extends Caio[C, V, L, A]

final private[caio] case class GetContextCaio[C, V, L]() extends Caio[C, V, L, C]

final private[caio] case class SetContextCaio[C, V, L](c:C) extends Caio[C, V, L, Unit]




sealed trait FoldCaio[C, V, L, +A] {
  def map[B](f:A => B):FoldCaio[C, V, L, B]

  def flatMap[B](f:A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]):FoldCaio[C, V, L, B]

  def prependLog(l:L)(implicit M:Monoid[L]):FoldCaio[C, V, L, A]
}

sealed trait FoldCaioPure[C, V, L, +A] extends FoldCaio[C, V, L, A] {
  def map[B](f:A => B):FoldCaioPure[C, V, L, B]

  def flatMap[B](f:A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]):FoldCaio[C, V, L, B]

  def prependLog(l:L)(implicit M:Monoid[L]):FoldCaioPure[C, V, L, A]
}
final private[caio] case class FoldCaioSuccess[C, V, L, A](c:C, l:L, a:A) extends FoldCaioPure[C, V, L, A] {
  def map[B](f: A => B): FoldCaioPure[C, V, L, B] = FoldCaioSuccess(c, l, f(a))

  def flatMap[B](f: A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]): FoldCaio[C, V, L, B] =
    if (M.isEmpty(l)) f(a)
    else f(a).prependLog(l)

  def prependLog(l: L)(implicit M: Monoid[L]): FoldCaioPure[C, V, L, A] = 
}
final private[caio] case class FoldCaioFailure[C, V, L, A](c:C, l:L, head:V, tail:List[V]) extends FoldCaioPure[C, V, L, Nothing] {
  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] = this

  def flatMap[B](f: Nothing => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] = this
}
final private[caio] case class FoldCaioError[C, V, L, A](c:C, l:L, e:Throwable) extends FoldCaioPure[C, V, L, Nothing] {

  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] = this

  def flatMap[B](f: Nothing => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] = this
}
final private[caio] case class FoldCaioIO[C, V, L, A](c:C, l:L, io:IO[FoldCaioPure[C, V, L, A]]) extends FoldCaio[C, V, L, A] {
  def map[B](f: A => B): FoldCaio[C, V, L, B] =
    FoldCaioIO[C, V, L, B](c, l, io.map(_.map(f)))
}
object Caio {

  def foldIO[C, V, L:Monoid, A](caio:Caio[C, V, L, A], c:C):FoldCaio[C, V, L, A]

  private def foldCaio[C, V, L, A](caio:Caio[C, V, L, A], c:C, l:L):FoldCaio[C, V, L, A] =
    caio match {
      case PureCaio(a) =>
        FoldCaioSuccess(c, l, a)
      case IOCaio(io) =>
        FoldCaioIO(c, l, io)
      case LogCaio(l) =>
        FoldCaioSuccess(c, l, ())
      case ErrorCaio(e) =>
        FoldCaioError(c, l, e)
      case FailureCaio(head, tail) =>
        FoldCaioFailure(c, l, head, tail)
      case KleisliCaio(f) =>
        foldCaio(f(c), c, l) //eval try catch
      case BindCaio(source, f) =>
        foldCaio(source, c, l).map



    }

}