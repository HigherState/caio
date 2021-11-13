package caio

import caio.std.{CaioApplicative, CaioConcurrent}
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.{Concurrent, Fiber, IO, Resource}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, TimeoutException}
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Caio[-C,  +L, +A] {
  def as[B](b: => B): Caio[C, L, B] =
    MapCaio[C, L, A, B](this, _ => b)

  def attempt: Caio[C, L, Either[Throwable, A]] =
    HandleErrorCaio(MapCaio[C, L, A, Either[Throwable, A]](this, Right.apply), ex => PureCaio(Left(ex)))

  def background[C1 <: C, L1 >: L](implicit
    M: Monoid[L1]
  ): Resource[Caio[C1, L1, *], Caio[C1, L1, A]] = {
    implicit val applicative = new CaioApplicative[C1, L1] {}
    Resource.make(start[C1, L1, A])(_.cancel).map(_.join)
  }

  def map[B](f: A => B): Caio[C, L, B] =
    MapCaio(this, f)

  def flatMap[C1 <: C, L1 >: L, B](f: A => Caio[C1, L1, B]): Caio[C1, L1, B] =
    BindCaio(this, f)

  def <*[C1 <: C, L1 >: L](fb: => Caio[C1, L1, Any]): Caio[C1, L1, A] =
    BindCaio[C1, L1, A, A](this, a => MapCaio[C1, L1, Any, A](fb, _ => a))

  def *>[C1 <: C, L1 >: L, B](fb: => Caio[C1, L1, B]): Caio[C1, L1, B] =
    BindCaio[C1, L1, A, B](this, _ => fb)

  def censor[L1 >: L](f: L1 => L1): Caio[C, L1, A] =
    CensorCaio(this, f)

  def clear[L1 >: L](implicit M: Monoid[L1]): Caio[C, L1, A] =
    censor[L1](_ => M.empty)

  def guarantee[C1 <: C, L1 >: L: Monoid](finalizer: Caio[C1, L1, Unit]): Caio[C1, L1, A] =
    guaranteeCase(_ => finalizer)

  def guaranteeCase[C1 <: C, L1 >: L: Monoid](
    finalizer: ExitCase[Throwable] => Caio[C1, L1, Unit]
  ): Caio[C1, L1, A] =
    new CaioBracket[C1, L1].guaranteeCase(this)(finalizer)

  def handleErrorWith[C1 <: C, L1 >: L, A1 >: A](
    f: Throwable => Caio[C1, L1, A1]
  ): Caio[C1, L1, A1] =
    HandleErrorCaio(this, f)

  def listen: Caio[C, L, (A, L)] =
    ListenCaio(this)

  def localContext[C1 <: C](f: C1 => C1): Caio[C1, L, A] =
    LocalContextCaio(this, f)

  def race[C1 <: C, L1 >: L, B](
    caio: Caio[C1, L1, B]
  )(implicit M: Monoid[L1], CS: ContextShift[IO]): Caio[C1, L1, Either[A, B]] =
    new CaioConcurrent[C1, L1].race(this, caio)

  def start[C1 <: C, L1 >: L, A1 >: A](implicit
    M: Monoid[L1],
    CS: ContextShift[IO]
  ): Caio[C1, L1, Fiber[Caio[C1, L1, *], A1]] =
    new CaioConcurrent[C1, L1].start(this)

  def tapError[C1 <: C, L1 >: L](f: Throwable => Caio[C1, L1, Any]): Caio[C1, L1, A] =
    handleErrorWith(ex => f(ex) *> Caio.raiseError(ex))

  def tapFailures[C1 <: C, L1 >: L](
    f: NonEmptyList[V1] => Caio[C1, L1, Any]
  ): Caio[C1, L1, A] =
    handleFailuresWith[C1, L1, A](fs => f(fs) *> Caio.failMany(fs))

  def timeoutTo[C1 <: C, L1 >: L, A1 >: A](duration: FiniteDuration, fallback: Caio[C1, L1, A1])(implicit
    M: Monoid[L1],
    timer: Timer[Caio[C1, L1, *]],
    CS: ContextShift[IO]
  ): Caio[C1, L1, A1] =
    Concurrent.timeoutTo[Caio[C1, L1, *], A1](this, duration, fallback)(new CaioConcurrent[C1, L1], timer)

  def timeout[C1 <: C, L1 >: L](
    duration: FiniteDuration
  )(implicit M: Monoid[L1], timer: Timer[Caio[C1, L1, *]], CS: ContextShift[IO]): Caio[C1, L1, A] =
    timeoutTo[C1, L1, A](duration, ErrorCaio(new TimeoutException(duration.toString)))

  @inline def void: Caio[C, L, Unit] =
    as(())

  /**
   * Exceptions in stack will get thrown,
   * Failures will get thrown as CaioUnhandledFailuresException
   * @param c
   * @param M
   * @return
   */
  def run[L1 >: L](c: C)(implicit M: Monoid[L1]): IO[A] =
    Caio.run[C, L1, A](this, c)

  def run[C1 <: C, L1 >: L](implicit M: Monoid[L1], ev: C1 =:= Any): IO[A] =
    Caio.run[Any, L1, A](this.asInstanceOf[Caio[Any, L1, A]], ())

  def runContext[C1 <: C, L1 >: L](
    c: C1
  )(implicit M: Monoid[L1]): IO[(C1, L1, Either[Throwable, A])] =
    Caio.foldIO[C1,  L1, A](this, c).map {
      case FoldCaioSuccess(cOut, l, a)          =>
        (cOut, l, Right(a))
      case FoldCaioError(cOut, l, ex)           =>
        (cOut, l, Left(ex))
    }

  @inline def provideContext[L1 >: L](c: C)(implicit M: Monoid[L1]): Caio[Any, L1, A] =
    IOCaio(run[L1](c))
}

final private[caio] case class PureCaio[+A](a: A) extends Caio[Any, Nothing, A]

final private[caio] class IOCaio[+A] private (val f: () => IO[A]) extends Caio[Any, Nothing, A]

object IOCaio {
  def apply[A](a: => IO[A])                          = new IOCaio(() => a)
  def unapply[A](io: IOCaio[A]): Option[() => IO[A]] = Some(io.f)
}

/**
 * Kliesli is on any so we can erase the context system
 * @param kleisli
 * @tparam C
 * @tparam L
 * @tparam A
 */
final private[caio] case class KleisliCaio[C, L, +A](kleisli: Any => IO[FoldCaioPure[C, L, A]])
    extends Caio[C, L, A]

final private[caio] case class MapCaio[-C, +L, E, +A](source: Caio[C, L, E], f: E => A) extends Caio[C, L, A]

final private[caio] case class BindCaio[-C, +L, E, +A](source: Caio[C, L, E], f: E => Caio[C, L, A])
    extends Caio[C, L, A]

final private[caio] case class ErrorCaio(e: Throwable) extends Caio[Any, Nothing, Nothing]

final private[caio] case class HandleErrorCaio[-C, +L, +A](source: Caio[C, L, A], f: Throwable => Caio[C, L, A])
    extends Caio[C, L, A]

final private[caio] case class SetCaio[+L](l: L) extends Caio[Any, L, Unit]

final private[caio] case class TellCaio[+L](l: L) extends Caio[Any, L, Unit]

final private[caio] case class ListenCaio[-C, +L, +A](source: Caio[C, L, A]) extends Caio[C, L, (A, L)]

final private[caio] case class CensorCaio[-C, L, +A](source: Caio[C, L, A], f: L => L) extends Caio[C, L, A]

final private[caio] case class GetContextCaio[C]() extends Caio[C, Nothing, C]

final private[caio] case class SetContextCaio[C](c: C) extends Caio[C, Nothing, Unit]

final private[caio] case class LocalContextCaio[C, L, A](source: Caio[C, L, A], f: C => C) extends Caio[C, L, A]

case class CaioUnhandledFailuresException[V](failure: NonEmptyList[V])
    extends Exception("Caio failures have not been handled.")

sealed trait FoldCaio[C, L, +A] {

  /**
   * Required for transforming context outside of the evaluation GADT
   * Can transform Error and Failed cases as well
   * @param f
   * @tparam C2
   * @return
   */
  def contextMap[C2](f: C => C2): FoldCaio[C2, L, A]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, L, B]): FoldCaio[C, L, B]

  def toIO: IO[FoldCaioPure[C, L, A]]

  def map[B](f: A => B): FoldCaio[C, L, B]

  /**
   * Required for transforming EventLog, cant use FlatMap
   * Can transform Error and Failed cases as well
   * @param f
   * @return
   */
  def mapL[B](f: L => L): FoldCaio[C, L, A]
}

sealed trait FoldCaioPure[C, L, +A] extends FoldCaio[C, L, A] {

  def c: C

  def l: L

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, A]

  def map[B](f: A => B): FoldCaioPure[C, L, B]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, L, B]): FoldCaio[C, L, B]

  def toIO: IO[FoldCaioPure[C, L, A]] =
    IO.pure(this)

  def mapL[B](f: L => L): FoldCaioPure[C, L, A]
}

final private[caio] case class FoldCaioSuccess[C, L, +A](c: C, l: L, a: A) extends FoldCaioPure[C, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, A] =
    this.copy(c = f(this.c))

  def map[B](f: A => B): FoldCaioPure[C, L, B] =
    this.copy(a = f(a))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, L, B]): FoldCaio[C, L, B] =
    f(c, l, a)

  def mapL[B](f: L => L): FoldCaioPure[C, L, A] =
    this.copy(l = f(l))
}

final private[caio] case class FoldCaioError[C, L, +A](c: C, l: L, e: Throwable)
    extends FoldCaioPure[C, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, Nothing] =
    this.copy(c = f(this.c))

  def map[B](f: Nothing => B): FoldCaioPure[C, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, L, B]): FoldCaio[C, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioIO[C, L, +A](io: IO[FoldCaioPure[C, L, A]]) extends FoldCaio[C, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioIO[C2, L, A] =
    FoldCaioIO[C2, L, A](io.map(_.contextMap(f)))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, L, B]): FoldCaio[C, L, B] = {
    val ioflatmap = io.flatMap(_.flatMap(f) match {
      case FoldCaioIO(io2)             =>
        io2
      case p: FoldCaioPure[C, L, B] =>
        IO.pure(p)
    })
    FoldCaioIO(ioflatmap)
  }

  def map[B](f: A => B): FoldCaio[C, L, B] =
    FoldCaioIO(io.map(_.map(f)))

  def mapL[B](f: L => L): FoldCaio[C, L, A] =
    FoldCaioIO(io.map(_.mapL(f)))

  def toIO: IO[FoldCaioPure[C, L, A]] =
    io
}

object Caio {
  def apply[A](thunk: => A): Caio[Any, Nothing, A] =
    IOCaio[A](IO.apply(thunk))

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[Any, Nothing, A] =
    IOCaio(IO.async(k))

  def asyncF[C, L: Monoid, A](k: (Either[Throwable, A] => Unit) => Caio[C, L, Unit]): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).map(_ => ()))
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, L, A](c, Monoid[L].empty, a))
    }

  def unit: Caio[Any, Nothing, Unit] =
    pure(())

  def pure[A](a: A): Caio[Any, Nothing, A] =
    PureCaio(a)

  def effect[A](thunk: => A): Caio[Any, Nothing, A] =
    apply(thunk)

  def raiseError(ex: Throwable): Caio[Any, Nothing, Nothing] =
    ErrorCaio(ex)

  def fromEither[A](either: => Either[Throwable, A]): Caio[Any, Nothing, A] =
    IOCaio(IO.fromEither(either))

  def fromFuture[C, L, A](caio: Caio[C, L, Future[A]])(implicit CS: ContextShift[IO]): Caio[C, L, A] =
    caio.flatMap(future => IOCaio(IO.fromFuture(IO(future))))

  def fromIOFuture[A](iof: IO[Future[A]])(implicit CS: ContextShift[IO]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(IO.fromFuture(iof))

  def fromTry[A](`try`: Try[A]): Caio[Any, Nothing, A] =
    IOCaio(IO.fromTry(`try`))

  def getContext[C]: Caio[C, Nothing, C] =
    GetContextCaio()

  def liftIO[A](io: IO[A]): Caio[Any, Nothing, A] =
    IOCaio(io)

  def liftF[F[_]: ConcurrentEffect, A](fa: F[A]): Caio[Any, Nothing, A] =
    IOCaio(ConcurrentEffect[F].toIO(fa))

  def modifyContext[C](f: C => C): Caio[C, Nothing, Unit] =
    getContext[C].flatMap(c => setContext(f(c)))

  def never: Caio[Any, Nothing, Nothing] =
    async(_ => ())

  def setContext[C](context: C): Caio[C, Nothing, Unit] =
    SetContextCaio(context)

  def race[C, L, A, B](fa: Caio[C, L, A], fb: Caio[C, L, B])(implicit
    M: Monoid[L],
    CS: ContextShift[IO]
  ): Caio[C, L, Either[A, B]] =
    fa.race(fb)

  def sleep[C, L](duration: FiniteDuration)(implicit timer: Timer[Caio[C, L, *]]): Caio[C, L, Unit] =
    timer.sleep(duration)

  @inline def tell[L](l: L): Caio[Any, L, Unit] =
    TellCaio[L](l)

  private[caio] def run[C, L: Monoid, A](caio: Caio[C, L, A], c: C): IO[A] =
    foldIO[C, L, A](caio, c).map {
      case FoldCaioSuccess(_, _, a)          =>
        a
      case FoldCaioError(_, _, ex)           =>
        throw ex
    }

  private[caio] def runFail[C, L: Monoid, A](caio: Caio[C, L, A], c: C): IO[Either[NonEmptyList[V], A]] =
    foldIO[C, L, A](caio, c).map {
      case FoldCaioSuccess(_, _, a)          =>
        Right(a)
      case FoldCaioError(_, _, ex)           =>
        throw ex
    }

  private[caio] def foldIO[C, L, A](caio: Caio[C, L, A], c: Any)(implicit
    M: Monoid[L]
  ): IO[FoldCaioPure[C, L, A]] = {
    type Continuation    = (Any, Any, Any) => Caio[Any, Any, Any]
    type ErrorRecovery   = (Any, Any, Throwable) => Caio[Any, Any,  Any]

    sealed trait Handler
    case class OnSuccess(f: Continuation)    extends Handler
    case class OnError(f: ErrorRecovery)     extends Handler

    def tryOrError(value: => Caio[Any, Any, Any]): Caio[Any, Any, Any]       =
      try value
      catch { case NonFatal(ex) => ErrorCaio(ex) }

    @tailrec def nextHandler(fs: List[Handler]): Option[(Continuation, List[Handler])] =
      fs match {
        case OnSuccess(c) :: cs => Some((c, cs))
        case _ :: cs            => nextHandler(cs)
        case Nil                => None
      }

    @tailrec def nextErrorHandler(fs: List[Handler]): Option[(ErrorRecovery, List[Handler])] =
      fs match {
        case OnError(c) :: cs => Some((c, cs))
        case _ :: cs          => nextErrorHandler(cs)
        case Nil              => None
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
    def safeFold(
      caio: Caio[Any, Any, Any],
      c: Any,
      l: Any,
      handlers: List[Handler]
    ): FoldCaio[Any, Any, Any] = {
      @tailrec def foldCaio(
        caio: Caio[Any,  Any, Any],
        c: Any,
        l: Any,
        handlers: List[Handler]
      ): FoldCaio[Any, Any, Any] =
        caio match {
          case PureCaio(a) =>
            nextHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, a)), c, l, fs)
              case None          =>
                FoldCaioSuccess(c, l, a)
            }
          case IOCaio(io)  =>
            //The IO monad will bring this back into stack safety
            FoldCaioIO(
              io().redeemWith(
                e => safeFold(ErrorCaio(e), c, l, handlers).toIO,
                a => safeFold(PureCaio(a), c, l, handlers).toIO
              )
            )

          case KleisliCaio(f) =>
            Try(f(c)) match {
              //Doesnt support Error or Failure handling
              case scala.util.Success(foldIO) =>
                FoldCaioIO {
                  foldIO
                    .flatMap {
                      case FoldCaioSuccess(c, l2, a)          =>
                        //The IO monad will bring this back into stack safety
                        safeFold(PureCaio(a), c, M.combine(l.asInstanceOf[L], l2.asInstanceOf[L]), handlers).toIO
                      case FoldCaioError(c, l2, ex)           =>
                        safeFold(ErrorCaio(ex), c, M.combine(l.asInstanceOf[L], l2.asInstanceOf[L]), handlers).toIO
                    }
                    .handleErrorWith(ex => safeFold(ErrorCaio(ex), c, l, handlers).toIO)
                }
              case scala.util.Failure(ex)     =>
                foldCaio(ErrorCaio(ex), c, l, handlers)
            }

          case MapCaio(source, f) =>
            foldCaio(source, c, l, OnSuccess((_, _, a) => PureCaio(f(a))) :: handlers)

          case BindCaio(source, f) =>
            foldCaio(source, c, l, OnSuccess((_, _, a) => f(a)) :: handlers)

          case ErrorCaio(e) =>
            nextErrorHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, e)), c, l, fs)
              case None          =>
                FoldCaioError(c, l, e)
            }

          case HandleErrorCaio(source, f) =>
            foldCaio(source, c, l, OnError((_, _, e) => f(e)) :: handlers)
            
          case SetCaio(l2) =>
            foldCaio(Caio.unit, c, l2, handlers)

          case TellCaio(l2) =>
            foldCaio(Caio.unit, c, M.combine(l.asInstanceOf[L], l2.asInstanceOf[L]), handlers)

          case ListenCaio(source)    =>
            foldCaio(source, c, l, OnSuccess((_, l, a) => PureCaio(a -> l)) :: handlers)

          case CensorCaio(source, f) =>
            foldCaio(source, c, l, OnSuccess((_, l, a) => BindCaio(SetCaio(f(l)), (_: Unit) => PureCaio(a))) :: handlers)

          case GetContextCaio() =>
            foldCaio(PureCaio(c), c, l, handlers)

          case SetContextCaio(replaceC) =>
            foldCaio(Caio.unit, replaceC, l, handlers)

          case LocalContextCaio(fa, f) =>
            foldCaio(
              fa,
              f(c),
              l,
              OnSuccess((_, _, a) => MapCaio(SetContextCaio(c), (_: Unit) => a)) ::
                OnError((_, _, e) => BindCaio(SetContextCaio(c), (_: Unit) => ErrorCaio(e))) ::

                handlers
            )
        }

      foldCaio(caio, c, l, handlers)
    }

    IO.suspend(
      safeFold(caio.asInstanceOf[Caio[Any, Any, Any]], c, M.empty, Nil).asInstanceOf[FoldCaio[C, L, A]].toIO
    )
  }
}
