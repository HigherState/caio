package caio

import cats.{Eq, Monoid}
import cats.data.NonEmptyList
import cats.effect.IO

sealed trait Caio[C, V, L, +A]

final private[caio] case class PureCaio[C, V, L, +A](a: A) extends Caio[C, V, L, A]

final private[caio] case class IOCaio[C, V, L, +A](a: IO[A]) extends Caio[C, V, L, A]

final private[caio] case class KleisliCaio[C, V, L, +A](kleisli: C => FoldCaio[C, V, L, A]) extends Caio[C, V, L, A]


final private[caio] case class MapCaio[C, V, L, E, +A](source: Caio[C, V, L, E], f: E => A) extends Caio[C, V, L, A]

final private[caio] case class BindCaio[C, V, L, E, +A](source: Caio[C, V, L, E], f: E => Caio[C, V, L, A]) extends Caio[C, V, L, A]



final private[caio] case class ErrorCaio[C, V, L](e:Throwable) extends Caio[C, V, L, Nothing]

final private[caio] case class HandleErrorCaio[C, V, L, +A](source: Caio[C, V, L, A], f: Throwable => Caio[C, V, L, A]) extends Caio[C, V, L, A]


final private[caio] case class FailureCaio[C, V, L](head:V, tail:List[V]) extends Caio[C, V, L, Nothing]

final private[caio] case class HandleFailureCaio[C, V, L, +A](source: Caio[C, V, L, A], f: NonEmptyList[V] => Caio[C, V, L, A]) extends Caio[C, V, L, A]


final private[caio] case class TellCaio[C, V, L](l:L) extends Caio[C, V, L, Unit]

final private[caio] case class ListenCaio[C, V, L, +A](source: Caio[C, V, L, A]) extends Caio[C, V, L, (A, L)]

final private[caio] case class CensorCaio[C, V, L, +A](source: Caio[C, V, L, A], f:L => L) extends Caio[C, V, L, A]


final private[caio] case class GetContextCaio[C, V, L]() extends Caio[C, V, L, C]

final private[caio] case class SetContextCaio[C, V, L](c:C) extends Caio[C, V, L, Unit]




sealed trait FoldCaio[C, V, L, +A] {
  def map[B](f:A => B):FoldCaio[C, V, L, B]

  def contextMap[C2](f:C => C2):FoldCaio[C2, V, L, A]

  def flatMap[B](f:A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]):FoldCaio[C, V, L, B]

  def toIO:IO[FoldCaioPure[C, V, L, A]]

  def mapL[B](f:L => L):FoldCaio[C, V, L, A]
}

sealed trait FoldCaioPure[C, V, L, +A] extends FoldCaio[C, V, L, A] {

  def c:C

  def l:L

  def map[B](f:A => B):FoldCaioPure[C, V, L, B]

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A]

  def flatMap[B](f:A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]):FoldCaio[C, V, L, B]

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    IO.pure(this)

  def mapL[B](f:L => L):FoldCaioPure[C, V, L, A]
}

final private[caio] case class FoldCaioSuccess[C, V, L, +A](c:C, l:L, a:A) extends FoldCaioPure[C, V, L, A] {
  def map[B](f: A => B): FoldCaioPure[C, V, L, B] = FoldCaioSuccess(c, l, f(a))

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A] =
    this.copy(c = f(this.c))

  def flatMap[B](f: A => FoldCaio[C, V, L, B])(implicit M:Monoid[L], E:Eq[L]): FoldCaio[C, V, L, B] =
    if (M.isEmpty(l)) f(a)
    else f(a).mapL(pl => M.combine(l, pl))

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, A] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioFailure[C, V, L, +A](c:C, l:L, head:V, tail:List[V]) extends FoldCaioPure[C, V, L, Nothing] {
  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] = this

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))

  def flatMap[B](f: Nothing => FoldCaio[C, V, L, B])(implicit M: Monoid[L], E: Eq[L]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioError[C, V, L, +A](c:C, l:L, e:Throwable) extends FoldCaioPure[C, V, L, Nothing] {

  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] = this

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))

  def flatMap[B](f: Nothing => FoldCaio[C, V, L, B])(implicit M: Monoid[L], E: Eq[L]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioIO[C, V, L, +A](io:IO[FoldCaioPure[C, V, L, A]]) extends FoldCaio[C, V, L, A] {
  def map[B](f: A => B): FoldCaio[C, V, L, B] =
    FoldCaioIO[C, V, L, B](io.map(_.map(f)))

  def contextMap[C2](f: C => C2): FoldCaio[C2, V, L, A] =
    FoldCaioIO[C2, V, L, A](io.map(_.contextMap(f)))

  def flatMap[B](f: A => FoldCaio[C, V, L, B])(implicit M: Monoid[L], E: Eq[L]): FoldCaio[C, V, L, B] = {
    val ioflatmap = io.flatMap(_.flatMap(f) match {
      case FoldCaioIO(io2) =>
        io2
      case p:FoldCaioPure[C, V, L, B] =>
        IO.pure(p)
    })
    FoldCaioIO(ioflatmap)
  }

  def mapL[B](f: L => L): FoldCaio[C, V, L, A] =
    FoldCaioIO(io.map(_.mapL(f)))

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    io
}

object Caio {

  def foldIO[C, V, L: Monoid : Eq, A](caio: Caio[C, V, L, A], c: C): IO[FoldCaioPure[C, V, L, A]] = {

    def foldCaio[B](caio: Caio[C, V, L, B], c: C, l: L): FoldCaio[C, V, L, B] =
      caio match {
        case PureCaio(a) =>
          FoldCaioSuccess(c, l, a)
        case IOCaio(io) =>
          FoldCaioIO(io.redeem(FoldCaioError(c, l, _), FoldCaioSuccess(c, l, _)))
        case KleisliCaio(f) =>
          f(c)

        case MapCaio(source, f) =>
          foldCaio(source, c, l).map(f)
        case BindCaio(source, f) =>
          foldCaio(source, c, l).flatMap(a => foldCaio(f(a), c, l))

        case ErrorCaio(e) =>
          FoldCaioError(c, l, e)
        case HandleErrorCaio(source, f) =>
          foldCaio(source, c, l) match {
            case FoldCaioError(c, l, e) =>
              foldCaio(f(e), c, l)
            case p: FoldCaioPure[C, V, L, _] =>
              p
            case FoldCaioIO(io) =>
              val newIO = io.flatMap {
                case FoldCaioError(c, l, e) =>
                  foldCaio(f(e), c, l) match {
                    case FoldCaioIO(io2) =>
                      io2
                    case p: FoldCaioPure[C, V, L, B] =>
                      IO.pure(p)
                  }
                case p: FoldCaioPure[C, V, L, B] =>
                  IO.pure(p)
              }
              FoldCaioIO(newIO)
          }

        case FailureCaio(head, tail) =>
          FoldCaioFailure(c, l, head, tail)
        case HandleFailureCaio(source, f) =>
          foldCaio(source, c, l) match {
            case FoldCaioFailure(c, l, head, tail) =>
              foldCaio(f(NonEmptyList(head, tail)), c, l)
            case p: FoldCaioPure[C, V, L, B] =>
              p
            case FoldCaioIO(io) =>
              val newIO = io.flatMap {
                case FoldCaioFailure(c, l, head, tail) =>
                  foldCaio(f(NonEmptyList(head, tail)), c, l) match {
                    case FoldCaioIO(io2) =>
                      io2
                    case p: FoldCaioPure[C, V, L, B] =>
                      IO.pure(p)
                  }
                case p: FoldCaioPure[C, V, L, B] =>
                  IO.pure(p)
              }
              FoldCaioIO(newIO)
          }

        case TellCaio(l) =>
          FoldCaioSuccess(c, l, ())

        case ListenCaio(source) =>
          foldCaio(source, c, l) match {
            case p: FoldCaioPure[C, V, L, _] =>
              p.map(a => a -> p.l)
            case FoldCaioIO(io) =>
              FoldCaioIO(io.map(p => p.map(a => a -> p.l)))
          }

        case CensorCaio(source, f) =>
          foldCaio(source, c, l).mapL(f)

        case GetContextCaio() =>
          FoldCaioSuccess[C, V, L, B](c, l, c.asInstanceOf[B]) //cant seem to remove that one

        case SetContextCaio(replaceC) =>
          FoldCaioSuccess[C, V, L, Unit](replaceC, l, ())
      }

    foldCaio(caio, c, Monoid.empty[L]).toIO
  }
}