package caio

import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.IO
import scala.annotation.tailrec
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

  def map[B](f: A => B): FoldCaio[C, V, L, B] =
    FoldCaioIO(io.map(_.map(f)))

  def mapL[B](f: L => L): FoldCaio[C, V, L, A] =
    FoldCaioIO(io.map(_.mapL(f)))

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    io
}

object Caio {

  private[caio] def foldIO[C, V, L, A](caio: Caio[C, V, L, A], c: C)(implicit M:Monoid[L]): IO[FoldCaioPure[C, V, L, A]] = {
    def tryOrError(value: => Caio[C, V, L, Any]): Caio[C, V, L, Any] =
      try value
      catch { case NonFatal(ex) => ErrorCaio(ex) }

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
    def safeFold(caio: Caio[C, V, L, Any], c: C, l: L, fs: List[Any => Caio[C, V, L, Any]]): FoldCaio[C, V, L, Any] = {
      @tailrec def foldCaio(caio: Caio[C, V, L, Any], c: C, l: L, fs: List[Any => Caio[C, V, L, Any]]): FoldCaio[C, V, L, Any] =
        (caio, fs) match {
          case (PureCaio(a), f :: fs) =>
            foldCaio(tryOrError(f(a)), c,  l, fs)

          case (PureCaio(a), Nil) =>
            FoldCaioSuccess(c, l, a)

          case (IOCaio(io), f :: fs) =>
            FoldCaioIO(io.redeemWith(
              e => IO.pure(FoldCaioError(c, l, e)),
              a => safeFold(tryOrError(f(a)), c, l, fs).toIO
            ))

          case (IOCaio(io), Nil) =>
            FoldCaioIO(io.redeem(FoldCaioError(c, l, _), FoldCaioSuccess(c, l, _)))

          case (KleisliCaio(f), ff :: fs) =>
            f(c) match {
              case FoldCaioSuccess(c, l, a) =>
                foldCaio(ff(a), c, l , fs)
              case failure: FoldCaioFailure[_, _, _, _] =>
                failure
              case error: FoldCaioError[_, _, _, _] =>
                error
              case fold: FoldCaioIO[_, _, _, _] =>
                fold.flatMap { case (c, l, a) =>
                  safeFold(ff(a), c,  l, fs)
                }
            }

          case (KleisliCaio(f), Nil) =>
            try f(c)
            catch { case NonFatal(ex) => FoldCaioError(c, l, ex) }

          case (MapCaio(source, f), list) =>
            foldCaio(source, c,  l, f.andThen(PureCaio[C, V, L, Any](_)) :: list)

          case (BindCaio(source, f), list) =>
            foldCaio(source, c, l, f :: list)

          case (ErrorCaio(e), _) =>
            FoldCaioError(c, l, e)

          case (HandleErrorCaio(source, f), list) =>
            safeFold(source, c, l, Nil) match {
              case FoldCaioError(c2, l2, e) =>
                foldCaio(tryOrError(f(e)), c2, l2, list)
              case p: FoldCaioPure[C, V, L, _] if list.isEmpty =>
                p
              case p: FoldCaioPure[C, V, L, _] =>
                p.flatMap { case (c, l, a) =>
                  val f :: fs = list 
                  safeFold(f(a), c,  l, fs)
                }
              case FoldCaioIO(io) =>
                FoldCaioIO {
                  io.flatMap {
                    case FoldCaioError(c2, l2, e) =>
                      safeFold(tryOrError(f(e)), c2, l2, list) match {
                        case FoldCaioIO(io2) =>
                          io2
                        case p: FoldCaioPure[C, V, L, _] =>
                          IO.pure(p)
                      }
                    case p: FoldCaioPure[C, V, L, _] if list.isEmpty =>
                      IO.pure(p)
                    case p: FoldCaioPure[C, V, L, _] =>
                      p.flatMap { case (c, l, a) =>
                        val f :: fs = list 
                        safeFold(f(a), c,  l, fs)
                      }.toIO
                  }
                }
            }

          case (FailureCaio(head, tail), _) =>
            FoldCaioFailure(c, l, head, tail)

          case (HandleFailureCaio(source, f), list) =>
            safeFold(source, c, l, Nil) match {
              case FoldCaioFailure(c2, l2, head, tail) =>
                foldCaio(tryOrError(f(NonEmptyList(head, tail))), c2, l2, list)
              case p: FoldCaioPure[C, V, L, _] if list.isEmpty =>
                p
              case p: FoldCaioPure[C, V, L, _] =>
                p.flatMap { case (c, l, a) =>
                  val f :: fs = list 
                  safeFold(f(a), c,  l, fs)
                }
              case FoldCaioIO(io) =>
                FoldCaioIO {
                  io.flatMap {
                    case FoldCaioFailure(c2, l2, head, tail) =>
                      safeFold(tryOrError(f(NonEmptyList(head, tail))), c2, l2, list) match {
                        case FoldCaioIO(io2) =>
                          io2
                        case p: FoldCaioPure[C, V, L, _] =>
                          IO.pure(p)
                      }
                    case p: FoldCaioPure[C, V, L, _] if list.isEmpty =>
                      IO.pure(p)
                    case p: FoldCaioPure[C, V, L, _] =>
                      p.flatMap { case (c, l, a) =>
                        val f :: fs = list 
                        safeFold(f(a), c,  l, fs)
                      }.toIO
                  }
                }
            }

          case (TellCaio(l2), f :: fs) =>
            foldCaio(f(()), c, M.combine(l, l2), fs)

          case (TellCaio(l2), Nil) =>
            FoldCaioSuccess(c, M.combine(l, l2), ())

          case (ListenCaio(source), list) =>
            foldCaio(source, c,  l, ((a: Any) => PureCaio[C, V, L, Any](a ->  l)) :: list)

          case (CensorCaio(source, f), list) =>
            safeFold(source, c, l, list).mapL(f)

          case (GetContextCaio(), f :: fs) =>
            foldCaio(f(c), c, l, fs)

          case (GetContextCaio(), Nil) =>
            FoldCaioSuccess(c, l, c)

          case (SetContextCaio(replaceC), f :: fs) =>
            foldCaio(f(()), replaceC, l, fs)

          case (SetContextCaio(replaceC), Nil) =>
            FoldCaioSuccess[C, V, L, Unit](replaceC, l, ())
        }

      foldCaio(caio, c, l, fs)
    }

    safeFold(caio, c, Monoid.empty[L], Nil).map(_.asInstanceOf[A]).toIO
  }
}