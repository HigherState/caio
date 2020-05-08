package caio

import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.IO

import scala.util.control.NonFatal

sealed trait Caio[C, V, L, +A] {

  /**
   * Exceptions in stack will get thrown,
   * Failures will get thrown as CaioUnhandledFailuresException
   * @param c
   * @param M
   * @return
   */
  def unsafeRun(c:C)(implicit M:Monoid[L]):A =
    Caio.foldIO(this, c).unsafeRunSync() match {
      case FoldCaioSuccess(_, _, a) =>
        a
      case FoldCaioFailure(_, _, head, tail) =>
        throw CaioUnhandledFailuresException(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex) =>
        throw ex
    }

  def unsafeRunFail(c:C)(implicit M:Monoid[L]):Either[NonEmptyList[V], A] =
    Caio.foldIO(this, c).unsafeRunSync() match {
      case FoldCaioSuccess(_, _, a) =>
        Right(a)
      case FoldCaioFailure(_, _, head, tail) =>
        Left(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex) =>
        throw ex
    }

  def unsafeRunContext(c:C)(implicit M:Monoid[L]):(C, L, Either[ErrorOrFailure[V], A]) =
    Caio.foldIO(this, c).unsafeRunSync() match {
      case FoldCaioSuccess(cOut, l, a) =>
        (cOut, l, Right(a))
      case FoldCaioFailure(cOut, l, head, tail) =>
        (cOut, l, Left(Right(NonEmptyList(head, tail))))
      case FoldCaioError(cOut, l, ex) =>
        (cOut, l, Left(Left(ex)))
    }
}

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


case class CaioUnhandledFailuresException[V](failure:NonEmptyList[V])
  extends Exception("Caio failures have not been handled.")



sealed trait FoldCaio[C, V, L, +A] {

  /**
   * Required for transforming context outside of the evaluation GADT
   * Can transform Error and Failed cases as well
   * @param f
   * @tparam C2
   * @return
   */
  def contextMap[C2](f:C => C2):FoldCaio[C2, V, L, A]

  def flatMap[B](f:(C, L, A) => FoldCaio[C, V, L, B]):FoldCaio[C, V, L, B]

  def toIO:IO[FoldCaioPure[C, V, L, A]]

  /**
   * Required for transforming EventLog, cant use FlatMap
   * Can transform Error and Failed cases as well
   * @param f
   * @return
   */
  def mapL[B](f:L => L):FoldCaio[C, V, L, A]
}

sealed trait FoldCaioPure[C, V, L, +A] extends FoldCaio[C, V, L, A] {

  def c:C

  def l:L

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A]

  def map[B](f:A => B):FoldCaioPure[C, V, L, B]

  def flatMap[B](f:(C, L, A) => FoldCaio[C, V, L, B]):FoldCaio[C, V, L, B]

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    IO.pure(this)

  def mapL[B](f:L => L):FoldCaioPure[C, V, L, A]
}

final private[caio] case class FoldCaioSuccess[C, V, L, +A](c:C, l:L, a:A) extends FoldCaioPure[C, V, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A] =
    this.copy(c = f(this.c))

  def map[B](f:A => B):FoldCaioPure[C, V, L, B] =
    this.copy(a = f(a))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    f(c, l, a)

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, A] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioFailure[C, V, L, +A](c:C, l:L, head:V, tail:List[V]) extends FoldCaioPure[C, V, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))


  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioError[C, V, L, +A](c:C, l:L, e:Throwable) extends FoldCaioPure[C, V, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))

  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioIO[C, V, L, +A](io:IO[FoldCaioPure[C, V, L, A]]) extends FoldCaio[C, V, L, A] {

  def contextMap[C2](f: C => C2): FoldCaio[C2, V, L, A] =
    FoldCaioIO[C2, V, L, A](io.map(_.contextMap(f)))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] = {
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

  private[caio] def foldIO[C, V, L, A](caio: Caio[C, V, L, A], c: C)(implicit M:Monoid[L]): IO[FoldCaioPure[C, V, L, A]] = {

    /**
     * Recursive fold of Caio GADT.
     * Requires Exception handling on function evaluation
     * Requires trampolining
     * @param caio
     * @param c
     * @param l
     * @tparam B
     * @return
     */
    def foldCaio[B](caio: Caio[C, V, L, B], c: C, l: L): FoldCaio[C, V, L, B] =
      caio match {
        case PureCaio(a) =>
          FoldCaioSuccess(c, l, a)

        case IOCaio(io) =>
          FoldCaioIO(io.redeem(FoldCaioError(c, l, _), FoldCaioSuccess(c, l, _)))

        case KleisliCaio(f) =>
          try f(c)
          catch { case NonFatal(ex) => FoldCaioError(c, l, ex) }

        case MapCaio(source, f) =>
          foldCaio(source, c, l)
            .flatMap{ (c2, l2, a) =>
              try FoldCaioSuccess(c2, l2, f(a))
              catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
            }

        case BindCaio(source, f) =>
          foldCaio(source, c, l)
            .flatMap{ (c2, l2, a) =>
              try foldCaio(f(a), c2, l2)
              catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
            }

        case ErrorCaio(e) =>
          FoldCaioError(c, l, e)

        case HandleErrorCaio(source, f) =>
          foldCaio(source, c, l) match {
            case FoldCaioError(c2, l2, e) =>
              try foldCaio(f(e), c2, l2)
              catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
            case p: FoldCaioPure[C, V, L, _] =>
              p
            case FoldCaioIO(io) =>
              val newIO = io.flatMap {
                case FoldCaioError(c2, l2, e) =>
                  val handled:FoldCaio[C, V, L, B] =
                    try foldCaio(f(e), c2, l2)
                    catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
                  handled match {
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
            case FoldCaioFailure(c2, l2, head, tail) =>
              try foldCaio(f(NonEmptyList(head, tail)), c2, l2)
              catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
            case p: FoldCaioPure[C, V, L, B] =>
              p
            case FoldCaioIO(io) =>
              val newIO = io.flatMap {
                case FoldCaioFailure(c2, l2, head, tail) =>
                  val handler:FoldCaio[C, V, L, B] =
                    try foldCaio(f(NonEmptyList(head, tail)), c2, l2)
                    catch { case NonFatal(ex) => FoldCaioError(c2, l2, ex) }
                  handler match {
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

        case TellCaio(l2) =>
          FoldCaioSuccess(c, M.combine(l, l2), ())

        case ListenCaio(source) =>
          foldCaio(source, c, l)
            .flatMap((c2, l2, a) => FoldCaioSuccess(c2, l2, a -> l2))

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