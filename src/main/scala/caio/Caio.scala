package caio

import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.IO
import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Caio[-C, V, L, +A] {

  def map[B](f:A => B):Caio[C, V, L, B] =
    MapCaio(this, f)

  def flatMap[C1 <: C, B](f: A => Caio[C1, V, L, B]): Caio[C1, V, L, B] =
    BindCaio(this, f)
  /**
   * Exceptions in stack will get thrown,
   * Failures will get thrown as CaioUnhandledFailuresException
   * @param c
   * @param M
   * @return
   */
  def run(c: C)(implicit M: Monoid[L]): IO[A] =
    Caio.foldIO(this, c).map {
      case FoldCaioSuccess(_, _, a) =>
        a
      case FoldCaioFailure(_, _, head, tail) =>
        throw CaioUnhandledFailuresException(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex) =>
        throw ex
    }

  def runFail(c: C)(implicit M: Monoid[L]): IO[Either[NonEmptyList[V], A]] =
    Caio.foldIO(this, c).map {
      case FoldCaioSuccess(_, _, a) =>
        Right(a)
      case FoldCaioFailure(_, _, head, tail) =>
        Left(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex) =>
        throw ex
    }

  def runContext[C1 <: C](c: C1)(implicit M: Monoid[L]): IO[(C1, L, Either[ErrorOrFailure[V], A])] =
    Caio.foldIO(this, c).map {
      case FoldCaioSuccess(cOut, l, a) =>
        (cOut, l, Right(a))
      case FoldCaioFailure(cOut, l, head, tail) =>
        (cOut, l, Left(Right(NonEmptyList(head, tail))))
      case FoldCaioError(cOut, l, ex) =>
        (cOut, l, Left(Left(ex)))
    }
}

final private[caio] case class PureCaio[V, L, +A](a: A) extends Caio[Any, V, L, A]

final private[caio] case class IOCaio[V, L, +A](a: IO[A]) extends Caio[Any, V, L, A]

final private[caio] case class KleisliCaio[C, V, L, +A](kleisli: C => FoldCaioIO[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class MapCaio[-C, V, L, E, +A](source: Caio[C, V, L, E], f: E => A) extends Caio[C, V, L, A]

final private[caio] case class BindCaio[-C, V, L, E, +A](source: Caio[C, V, L, E], f: E => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class ErrorCaio[V, L](e: Throwable) extends Caio[Any, V, L, Nothing]

final private[caio] case class HandleErrorCaio[-C, V, L, +A](source: Caio[C, V, L, A], f: Throwable => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class FailureCaio[V, L](head: V, tail: List[V]) extends Caio[Any, V, L, Nothing]

final private[caio] case class HandleFailureCaio[-C, V, L, +A](source: Caio[C, V, L, A], f: NonEmptyList[V] => Caio[C, V, L, A]) extends Caio[C, V, L, A]

final private[caio] case class SetCaio[V, L](l: L) extends Caio[Any, V, L, Unit]

final private[caio] case class TellCaio[V, L](l: L) extends Caio[Any, V, L, Unit]

final private[caio] case class ListenCaio[-C, V, L, +A](source: Caio[C, V, L, A]) extends Caio[C, V, L, (A, L)]

final private[caio] case class CensorCaio[-C, V, L, +A](source: Caio[C, V, L, A], f: L => L) extends Caio[C, V, L, A]

final private[caio] case class GetContextCaio[C, V, L]() extends Caio[C, V, L, C]

final private[caio] case class SetContextCaio[C, V, L](c: C) extends Caio[C, V, L, Unit]


case class CaioUnhandledFailuresException[V](failure: NonEmptyList[V])
  extends Exception("Caio failures have not been handled.")



sealed trait FoldCaio[C, V, L, +A] {

  /**
   * Required for transforming context outside of the evaluation GADT
   * Can transform Error and Failed cases as well
   * @param f
   * @tparam C2
   * @return
   */
  def contextMap[C2](f: C => C2):FoldCaio[C2, V, L, A]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B]

  def toIO:IO[FoldCaioPure[C, V, L, A]]

  def map[B](f: A => B): FoldCaio[C, V, L, B]

  /**
   * Required for transforming EventLog, cant use FlatMap
   * Can transform Error and Failed cases as well
   * @param f
   * @return
   */
  def mapL[B](f:L => L):FoldCaio[C, V, L, A]
}

sealed trait FoldCaioPure[C, V, L, +A] extends FoldCaio[C, V, L, A] {

  def c: C

  def l: L

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A]

  def map[B](f:A => B):FoldCaioPure[C, V, L, B]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B]

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    IO.pure(this)

  def mapL[B](f:L => L):FoldCaioPure[C, V, L, A]
}

final private[caio] case class FoldCaioSuccess[C, V, L, +A](c: C, l: L, a: A) extends FoldCaioPure[C, V, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A] =
    this.copy(c = f(this.c))

  def map[B](f:A => B):FoldCaioPure[C, V, L, B] =
    this.copy(a = f(a))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    f(c, l, a)

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, A] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioFailure[C, V, L, +A](c: C, l: L, head: V, tail: List[V]) extends FoldCaioPure[C, V, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))


  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioError[C, V, L, +A](c: C, l: L, e: Throwable) extends FoldCaioPure[C, V, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))

  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioIO[C, V, L, +A](io: IO[FoldCaioPure[C, V, L, A]]) extends FoldCaio[C, V, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioIO[C2, V, L, A] =
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

  def map[B](f: A => B): FoldCaio[C, V, L, B] =
    FoldCaioIO(io.map(_.map(f)))

  def mapL[B](f: L => L): FoldCaio[C, V, L, A] =
    FoldCaioIO(io.map(_.mapL(f)))

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    io
}

object Caio {
  def unit[C, V, L]: Caio[C, V, L, Unit] =
    pure(())

  def pure[C, V, L, A](a:A): Caio[C, V, L, A] =
    PureCaio(a)

  def raiseError[C, V, L](ex:Throwable): Caio[C, V, L, Nothing] =
    ErrorCaio(ex)

  def fail[C, V, L](failure:V, failures:V*): Caio[C, V, L,  Nothing] =
    FailureCaio(failure, failures.toList)

  def failMany[C, V, L](failures:NonEmptyList[V]): Caio[C, V, L,  Nothing] =
    FailureCaio(failures.head, failures.tail)

  private[caio] def foldIO[C, V, L, A](caio: Caio[C, V, L, A], c: C)(implicit M: Monoid[L]): IO[FoldCaioPure[C, V, L, A]] = {
    type Continuation = (Any, L, Any) => Caio[Any, V, L, Any]
    type ErrorRecovery = (Any, L, Throwable) => Caio[Any, V, L, Any]
    type FailureRecovery = (Any, L, NonEmptyList[V]) => Caio[Any, V, L, Any]

    sealed trait Handler
    case class OnSuccess(f: Continuation) extends Handler
    case class OnFailure(f: FailureRecovery) extends Handler
    case class OnError(f: ErrorRecovery) extends Handler

    def tryOrError(value: => Caio[Any, V, L, Any]): Caio[Any, V, L, Any] =
      try value
      catch { case NonFatal(ex) => ErrorCaio(ex) }

    @tailrec def nextHandler(fs: List[Handler]): Option[(Continuation, List[Handler])] =
      fs match {
        case OnSuccess(c) :: cs => Some((c, cs))
        case _ :: cs => nextHandler(cs)
        case Nil => None
      }

    @tailrec def nextErrorHandler(fs: List[Handler]): Option[(ErrorRecovery, List[Handler])] =
      fs match {
        case OnError(c) :: cs => Some((c, cs))
        case _ :: cs => nextErrorHandler(cs)
        case Nil => None
      }

    @tailrec def nextFailureHandler(fs: List[Handler]): Option[(FailureRecovery, List[Handler])] =
      fs match {
        case OnFailure(c) :: cs => Some((c, cs))
        case _ :: cs => nextFailureHandler(cs)
        case Nil => None
      }

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
    def safeFold(caio: Caio[Any, V, L, Any], c: Any, l: L, handlers: List[Handler]): FoldCaio[Any, V, L, Any] = {
      @tailrec def foldCaio(caio: Caio[Any, V, L, Any], c: Any, l: L, handlers: List[Handler]): FoldCaio[Any, V, L, Any] =
        caio match {
          case PureCaio(a) =>
            nextHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, a)), c,  l, fs)
              case None =>
                FoldCaioSuccess(c, l, a)
            }
          case IOCaio(io) =>
            //The IO monad will bring this back into stack safety
            FoldCaioIO(io.redeemWith(
              e => safeFold(ErrorCaio(e), c, l, handlers).toIO,
              a => safeFold(PureCaio(a), c, l, handlers).toIO
            ))

          case KleisliCaio(f) =>
            Try(f(c)) match {
                //Doesnt support Error or Failure handling
              case scala.util.Success(foldIO) =>
                FoldCaioIO {
                  foldIO.io.flatMap {
                    case FoldCaioSuccess(c, l2, a) =>
                      //The IO monad will bring this back into stack safety
                      safeFold(PureCaio(a), c, M.combine(l, l2), handlers).toIO
                    case FoldCaioFailure(c, l2, head, tail) =>
                      safeFold(FailureCaio(head, tail), c, M.combine(l, l2), handlers).toIO
                    case FoldCaioError(c, l2, ex) =>
                      safeFold(ErrorCaio(ex), c, M.combine(l, l2), handlers).toIO
                  }
                }
              case scala.util.Failure(ex) =>
                foldCaio(ErrorCaio(ex), c, l, handlers)
            }

          case MapCaio(source, f) =>
            foldCaio(source, c,  l, OnSuccess((_, _, a) => PureCaio(f(a))) :: handlers)

          case BindCaio(source, f) =>
            foldCaio(source, c, l, OnSuccess((_, _, a) => f(a)) :: handlers)

          case ErrorCaio(e) =>
            nextErrorHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, e)), c,  l, fs)
              case None =>
                FoldCaioError(c, l, e)
            }

          case HandleErrorCaio(source, f) =>
            foldCaio(source, c, l, OnError((_, _, e) => f(e)) :: handlers)

          case FailureCaio(head, tail) =>
            nextFailureHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, NonEmptyList(head, tail))), c, l, fs)
              case None =>
                FoldCaioFailure(c, l, head, tail)
            }

          case HandleFailureCaio(source, f) =>
            foldCaio(source, c, l, OnFailure((_, _, e) => f(e)) :: handlers)

          case SetCaio(l2) =>
            foldCaio(Caio.unit, c, l2, handlers)

          case TellCaio(l2) =>
            foldCaio(Caio.unit, c, M.combine(l, l2), handlers)

          case ListenCaio(source) =>
            foldCaio(source, c,  l, OnSuccess((_, l, a) => PureCaio(a -> l)) :: handlers)

          case CensorCaio(source, f) =>
            foldCaio(source, c, l, OnSuccess((_, l, a) => BindCaio(SetCaio(f(l)), (_:Unit) => PureCaio(a))) :: handlers)

          case GetContextCaio() =>
            foldCaio(PureCaio(c), c, l, handlers)

          case SetContextCaio(replaceC) =>
            foldCaio(Caio.unit, replaceC, l, handlers)
        }

      foldCaio(caio, c, l, handlers)
    }

    safeFold(caio.asInstanceOf[Caio[Any, V, L, Any]], c, M.empty, Nil).asInstanceOf[FoldCaio[C, V, L, A]].toIO
  }
}