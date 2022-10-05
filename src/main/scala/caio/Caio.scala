package caio

import caio.std.{CaioApplicative, CaioBracket, CaioConcurrent}
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, ExitCase, Fiber, IO, Resource, Timer}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, TimeoutException}
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Caio[-C, +V, +L, +A] {
  @inline def as[B](b: => B): Caio[C, V, L, B] =
    MapCaio[C, V, L, A, B](this, _ => b)

  @inline def attempt: Caio[C, V, L, Either[Throwable, A]] =
    HandleErrorCaio(MapCaio[C, V, L, A, Either[Throwable, A]](this, Right.apply), ex => PureCaio(Left(ex)))

  def background[C1 <: C, V1 >: V, L1 >: L](implicit
    M: Monoid[L1],
    CS: ContextShift[IO]
  ): Resource[Caio[C1, V1, L1, *], Caio[C1, V1, L1, A]] = {
    implicit val applicative = new CaioApplicative[C1, V1, L1] {}
    Resource.make(start[C1, V1, L1, A])(_.cancel).map(_.join)
  }

  def bracket[C1 <: C, V1 >: V, L1 >: L, A1 >: A, B](use: A1 => Caio[C1, V1, L1, B])(
    release: A1 => Caio[C1, V1, L1, Unit]
  )(implicit M: Monoid[L1]): Caio[C1, V1, L1, B] =
    new CaioBracket[C1, V1, L1].bracket[A1, B](this)(use)(release)

  def bracketCase[C1 <: C, V1 >: V, L1 >: L, A1 >: A, B](use: A1 => Caio[C1, V1, L1, B])(
    release: (A1, ExitCase[Throwable]) => Caio[C1, V1, L1, Unit]
  )(implicit M: Monoid[L1]): Caio[C1, V1, L1, B] =
    new CaioBracket[C1, V1, L1].bracketCase[A1, B](this)(use)(release)

  @inline def map[B](f: A => B): Caio[C, V, L, B] =
    MapCaio(this, f)

  @inline def flatMap[C1 <: C, V1 >: V, L1 >: L, B](f: A => Caio[C1, V1, L1, B]): Caio[C1, V1, L1, B] =
    BindCaio(this, f)

  @inline def <*[C1 <: C, V1 >: V, L1 >: L](fb: => Caio[C1, V1, L1, Any]): Caio[C1, V1, L1, A] =
    BindCaio[C1, V1, L1, A, A](this, a => MapCaio[C1, V1, L1, Any, A](fb, _ => a))

  @inline def *>[C1 <: C, V1 >: V, L1 >: L, B](fb: => Caio[C1, V1, L1, B]): Caio[C1, V1, L1, B] =
    BindCaio[C1, V1, L1, A, B](this, _ => fb)

  @inline def censor[L1 >: L](f: L1 => L1): Caio[C, V, L1, A] =
    CensorCaio(this, f)

  @inline def clear[L1 >: L](implicit M: Monoid[L1]): Caio[C, V, L1, A] =
    censor[L1](_ => M.empty)

  def guarantee[C1 <: C, V1 >: V, L1 >: L: Monoid](finalizer: Caio[C1, V1, L1, Unit]): Caio[C1, V1, L1, A] =
    guaranteeCase(_ => finalizer)

  def guaranteeCase[C1 <: C, V1 >: V, L1 >: L: Monoid](
    finalizer: ExitCase[Throwable] => Caio[C1, V1, L1, Unit]
  ): Caio[C1, V1, L1, A] =
    new CaioBracket[C1, V1, L1].guaranteeCase(this)(finalizer)

  @inline def handleErrorWith[C1 <: C, V1 >: V, L1 >: L, A1 >: A](
    f: Throwable => Caio[C1, V1, L1, A1]
  ): Caio[C1, V1, L1, A1] =
    HandleErrorCaio(this, f)

  @inline def handleFailuresWith[C1 <: C, V1 >: V, V2, L1 >: L, A1 >: A](
    f: NonEmptyList[V1] => Caio[C1, V2, L1, A1]
  ): Caio[C1, V2, L1, A1] =
    HandleFailureCaio(this, f)

  @inline def listen: Caio[C, V, L, (A, L)] =
    ListenCaio(this)

  @inline def localContext[C1 <: C](f: C1 => C1): Caio[C1, V, L, A] =
    LocalContextCaio(this, f)

  @inline def either: Caio[C, Nothing, L, Either[NonEmptyList[V], A]] =
    HandleFailureCaio[C, V, Nothing, L, Either[NonEmptyList[V], A]](
      MapCaio[C, V, L, A, Either[NonEmptyList[V], A]](this, Right.apply),
      fs => PureCaio(Left(fs))
    )

  def race[C1 <: C, V1 >: V, L1 >: L, B](
    caio: Caio[C1, V1, L1, B]
  )(implicit M: Monoid[L1], CS: ContextShift[IO]): Caio[C1, V1, L1, Either[A, B]] =
    new CaioConcurrent[C1, V1, L1].race(this, caio)

  def start[C1 <: C, V1 >: V, L1 >: L, A1 >: A](implicit
    M: Monoid[L1],
    CS: ContextShift[IO]
  ): Caio[C1, V1, L1, Fiber[Caio[C1, V1, L1, *], A1]] =
    new CaioConcurrent[C1, V1, L1].start(this)

  @inline def tapError[C1 <: C, V1 >: V, L1 >: L](f: Throwable => Caio[C1, V1, L1, Any]): Caio[C1, V1, L1, A] =
    handleErrorWith(ex => f(ex) *> Caio.raiseError(ex))

  @inline def tapFailures[C1 <: C, V1 >: V, L1 >: L](
    f: NonEmptyList[V1] => Caio[C1, V1, L1, Any]
  ): Caio[C1, V1, L1, A] =
    handleFailuresWith[C1, V1, V1, L1, A](fs => f(fs) *> Caio.failMany(fs))

  def timeoutTo[C1 <: C, V1 >: V, L1 >: L, A1 >: A](duration: FiniteDuration, fallback: Caio[C1, V1, L1, A1])(implicit
    M: Monoid[L1],
    timer: Timer[Caio[C1, V1, L1, *]],
    CS: ContextShift[IO]
  ): Caio[C1, V1, L1, A1] =
    Concurrent.timeoutTo[Caio[C1, V1, L1, *], A1](this, duration, fallback)(new CaioConcurrent[C1, V1, L1], timer)

  def timeout[C1 <: C, V1 >: V, L1 >: L](
    duration: FiniteDuration
  )(implicit M: Monoid[L1], timer: Timer[Caio[C1, V1, L1, *]], CS: ContextShift[IO]): Caio[C1, V1, L1, A] =
    timeoutTo[C1, V1, L1, A](duration, ErrorCaio(new TimeoutException(duration.toString)))

  @inline def void: Caio[C, V, L, Unit] =
    as(())

  /**
   * Exceptions in stack will get thrown,
   * Failures will get thrown as CaioUnhandledFailuresException
   * @param c
   * @param M
   * @return
   */
  def run[L1 >: L](c: C)(implicit M: Monoid[L1]): IO[A] =
    Caio.run[C, V, L1, A](this, c)

  def run[C1 <: C, L1 >: L](implicit M: Monoid[L1], ev: C1 =:= Any): IO[A] =
    Caio.run[Any, V, L1, A](this.asInstanceOf[Caio[Any, V, L1, A]], ())

  def runFail[L1 >: L](c: C)(implicit M: Monoid[L1]): IO[Either[NonEmptyList[V], A]] =
    Caio.runFail[C, V, L1, A](this, c)

  def runFail[C1 <: C, L1 >: L](implicit M: Monoid[L1], ev: C1 =:= Any): IO[Either[NonEmptyList[V], A]] =
    Caio.runFail[Any, V, L1, A](this.asInstanceOf[Caio[Any, V, L1, A]], ())

  def runContext[C1 <: C, V1 >: V, L1 >: L](
    c: C1
  )(implicit M: Monoid[L1]): IO[(C1, L1, Either[ErrorOrFailure[V1], A])] =
    Caio.foldIO[C1, V, L1, A](this, c).map {
      case FoldCaioSuccess(cOut, l, a)          =>
        (cOut, l, Right(a))
      case FoldCaioFailure(cOut, l, head, tail) =>
        (cOut, l, Left(Right(NonEmptyList(head, tail))))
      case FoldCaioError(cOut, l, ex)           =>
        (cOut, l, Left(Left(ex)))
    }

  @inline def provideContext[L1 >: L](c: C)(implicit M: Monoid[L1]): Caio[Any, V, L1, A] =
    IOCaio(run[L1](c))
}

final private[caio] case class PureCaio[+A](a: A) extends Caio[Any, Nothing, Nothing, A]

final private[caio] class IOCaio[+A] private (val f: () => IO[A]) extends Caio[Any, Nothing, Nothing, A]

object IOCaio {
  def apply[A](a: => IO[A])                          = new IOCaio(() => a)
  def unapply[A](io: IOCaio[A]): Option[() => IO[A]] = Some(io.f)
}

final private[caio] case class KleisliCaio[C, V, L, +A](kleisli: C => IO[FoldCaioPure[C, V, L, A]])
    extends Caio[C, V, L, A]

final private[caio] case class MapCaio[-C, +V, +L, E, +A](source: Caio[C, V, L, E], f: E => A) extends Caio[C, V, L, A]

final private[caio] case class BindCaio[-C, +V, +L, E, +A](source: Caio[C, V, L, E], f: E => Caio[C, V, L, A])
    extends Caio[C, V, L, A]

final private[caio] case class ErrorCaio(e: Throwable) extends Caio[Any, Nothing, Nothing, Nothing]

final private[caio] case class HandleErrorCaio[-C, +V, +L, +A](source: Caio[C, V, L, A], f: Throwable => Caio[C, V, L, A])
    extends Caio[C, V, L, A]

final private[caio] case class FailureCaio[+V](head: V, tail: List[V]) extends Caio[Any, V, Nothing, Nothing]

final private[caio] case class HandleFailureCaio[-C, V, V1, +L, +A](
  source: Caio[C, V, L, A],
  f: NonEmptyList[V] => Caio[C, V1, L, A]
) extends Caio[C, V1, L, A]

final private[caio] case class SetCaio[+L](l: L) extends Caio[Any, Nothing, L, Unit]

final private[caio] case class TellCaio[+L](l: L) extends Caio[Any, Nothing, L, Unit]

final private[caio] case class ListenCaio[-C, +V, +L, +A](source: Caio[C, V, L, A]) extends Caio[C, V, L, (A, L)]

final private[caio] case class CensorCaio[-C, +V, L, +A](source: Caio[C, V, L, A], f: L => L) extends Caio[C, V, L, A]

final private[caio] case class GetContextCaio[C]() extends Caio[C, Nothing, Nothing, C]

final private[caio] case class SetContextCaio[C](c: C) extends Caio[C, Nothing, Nothing, Unit]

final private[caio] case class LocalContextCaio[C, V, L, A](source: Caio[C, V, L, A], f: C => C) extends Caio[C, V, L, A]

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
  def contextMap[C2](f: C => C2): FoldCaio[C2, V, L, A]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B]

  def toIO: IO[FoldCaioPure[C, V, L, A]]

  def map[B](f: A => B): FoldCaio[C, V, L, B]

  /**
   * Required for transforming EventLog, cant use FlatMap
   * Can transform Error and Failed cases as well
   * @param f
   * @return
   */
  def mapL[B](f: L => L): FoldCaio[C, V, L, A]
}

sealed trait FoldCaioPure[C, V, L, +A] extends FoldCaio[C, V, L, A] {

  def c: C

  def l: L

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A]

  def map[B](f: A => B): FoldCaioPure[C, V, L, B]

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B]

  def toIO: IO[FoldCaioPure[C, V, L, A]] =
    IO.pure(this)

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, A]
}

final private[caio] case class FoldCaioSuccess[C, V, L, +A](c: C, l: L, a: A) extends FoldCaioPure[C, V, L, A] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, A] =
    this.copy(c = f(this.c))

  def map[B](f: A => B): FoldCaioPure[C, V, L, B] =
    this.copy(a = f(a))

  def flatMap[B](f: (C, L, A) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    f(c, l, a)

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, A] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioFailure[C, V, L, +A](c: C, l: L, head: V, tail: List[V])
    extends FoldCaioPure[C, V, L, Nothing] {

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, V, L, Nothing] =
    this.copy(c = f(this.c))

  def map[B](f: Nothing => B): FoldCaioPure[C, V, L, B] =
    this

  def flatMap[B](f: (C, L, Nothing) => FoldCaio[C, V, L, B]): FoldCaio[C, V, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, V, L, Nothing] =
    this.copy(l = f(l))
}
final private[caio] case class FoldCaioError[C, V, L, +A](c: C, l: L, e: Throwable)
    extends FoldCaioPure[C, V, L, Nothing] {

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
      case FoldCaioIO(io2)             =>
        io2
      case p: FoldCaioPure[C, V, L, B] =>
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
  @inline def apply[A](thunk: => A): Caio[Any, Nothing, Nothing, A] =
    IOCaio[A](IO.apply(thunk))

  @inline def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[Any, Nothing, Nothing, A] =
    IOCaio(IO.async(k))

  @inline def asyncF[C, V, L: Monoid, A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
    KleisliCaio[C, V, L, A] { c =>
      val k2 = k.andThen(c0 => Caio.foldIO(c0, c).map(_ => ()))
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, V, L, A](c, Monoid[L].empty, a))
    }

  @inline def unit: Caio[Any, Nothing, Nothing, Unit] =
    pure(())

  @inline def pure[A](a: A): Caio[Any, Nothing, Nothing, A] =
    PureCaio(a)

  @inline def effect[A](thunk: => A): Caio[Any, Nothing, Nothing, A] =
    apply(thunk)

  @inline def raiseError(ex: Throwable): Caio[Any, Nothing, Nothing, Nothing] =
    ErrorCaio(ex)

  @inline def fail[V](failure: V, failures: V*): Caio[Any, V, Nothing, Nothing] =
    FailureCaio(failure, failures.toList)

  @inline def failMany[V](failures: NonEmptyList[V]): Caio[Any, V, Nothing, Nothing] =
    FailureCaio(failures.head, failures.tail)

  @inline def fromEither[A](either: => Either[Throwable, A]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(IO.fromEither(either))

  @inline def fromEitherFailure[V, A](either: => Either[V, A]): Caio[Any, V, Nothing, A] =
    Try(either).fold(ErrorCaio(_), _.fold(FailureCaio(_, Nil), PureCaio(_)))

  def fromFuture[C, V, L, A](caio: Caio[C, V, L, Future[A]])(implicit CS: ContextShift[IO]): Caio[C, V, L, A] =
    caio.flatMap(future => IOCaio(IO.fromFuture(IO(future))))

  @inline def fromIOFuture[A](iof: IO[Future[A]])(implicit CS: ContextShift[IO]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(IO.fromFuture(iof))

  @inline def fromTry[A](`try`: Try[A]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(IO.fromTry(`try`))

  @inline def getContext[C]: Caio[C, Nothing, Nothing, C] =
    GetContextCaio()

  @inline def liftIO[A](io: IO[A]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(io)

  @inline def liftF[F[_]: ConcurrentEffect, A](fa: F[A]): Caio[Any, Nothing, Nothing, A] =
    IOCaio(ConcurrentEffect[F].toIO(fa))

  @inline def modifyContext[C](f: C => C): Caio[C, Nothing, Nothing, Unit] =
    getContext[C].flatMap(c => setContext(f(c)))

  @inline def never: Caio[Any, Nothing, Nothing, Nothing] =
    async(_ => ())

  @inline def setContext[C](context: C): Caio[C, Nothing, Nothing, Unit] =
    SetContextCaio(context)

  def race[C, V, L, A, B](fa: Caio[C, V, L, A], fb: Caio[C, V, L, B])(implicit
    M: Monoid[L],
    CS: ContextShift[IO]
  ): Caio[C, V, L, Either[A, B]] =
    fa.race(fb)

  def sleep[C, V, L](duration: FiniteDuration)(implicit timer: Timer[Caio[C, V, L, *]]): Caio[C, V, L, Unit] =
    timer.sleep(duration)

  @inline def tell[L](l: L): Caio[Any, Nothing, L, Unit] =
    TellCaio[L](l)

  private[caio] def run[C, V, L: Monoid, A](caio: Caio[C, V, L, A], c: C): IO[A] =
    foldIO[C, V, L, A](caio, c).map {
      case FoldCaioSuccess(_, _, a)          =>
        a
      case FoldCaioFailure(_, _, head, tail) =>
        throw CaioUnhandledFailuresException(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex)           =>
        throw ex
    }

  private[caio] def runFail[C, V, L: Monoid, A](caio: Caio[C, V, L, A], c: C): IO[Either[NonEmptyList[V], A]] =
    foldIO[C, V, L, A](caio, c).map {
      case FoldCaioSuccess(_, _, a)          =>
        Right(a)
      case FoldCaioFailure(_, _, head, tail) =>
        Left(NonEmptyList(head, tail))
      case FoldCaioError(_, _, ex)           =>
        throw ex
    }

  private[caio] def foldIO[C, V, L, A](caio: Caio[C, V, L, A], c: C)(implicit
    M: Monoid[L]
  ): IO[FoldCaioPure[C, V, L, A]] = {
    type Continuation    = (Any, Any, Any) => Caio[Any, Any, Any, Any]
    type ErrorRecovery   = (Any, Any, Throwable) => Caio[Any, Any, Any, Any]
    type FailureRecovery = (Any, Any, NonEmptyList[Any]) => Caio[Any, Any, Any, Any]

    sealed trait Handler
    case class OnSuccess(f: Continuation)    extends Handler
    case class OnFailure(f: FailureRecovery) extends Handler
    case class OnError(f: ErrorRecovery)     extends Handler

    def tryOrError(value: => Caio[Any, Any, Any, Any]): Caio[Any, Any, Any, Any]       =
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

    @tailrec def nextFailureHandler(fs: List[Handler]): Option[(FailureRecovery, List[Handler])] =
      fs match {
        case OnFailure(c) :: cs => Some((c, cs))
        case _ :: cs            => nextFailureHandler(cs)
        case Nil                => None
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
      caio: Caio[Any, Any, Any, Any],
      c: Any,
      l: Any,
      handlers: List[Handler]
    ): FoldCaio[Any, Any, Any, Any] = {
      @tailrec def foldCaio(
        caio: Caio[Any, Any, Any, Any],
        c: Any,
        l: Any,
        handlers: List[Handler]
      ): FoldCaio[Any, Any, Any, Any] =
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
                      case FoldCaioFailure(c, l2, head, tail) =>
                        safeFold(
                          FailureCaio(head, tail),
                          c,
                          M.combine(l.asInstanceOf[L], l2.asInstanceOf[L]),
                          handlers
                        ).toIO
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

          case FailureCaio(head, tail) =>
            nextFailureHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, l, NonEmptyList(head, tail))), c, l, fs)
              case None          =>
                FoldCaioFailure(c, l, head, tail)
            }

          case HandleFailureCaio(source, f) =>
            foldCaio(source, c, l, OnFailure((_, _, e) => f(e)) :: handlers)

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
                OnFailure((_, _, e) => BindCaio(SetContextCaio(c), (_: Unit) => FailureCaio(e.head, e.tail))) ::
                handlers
            )
        }

      foldCaio(caio, c, l, handlers)
    }

    IO.suspend(
      safeFold(caio.asInstanceOf[Caio[Any, Any, Any, Any]], c, M.empty, Nil).asInstanceOf[FoldCaio[C, V, L, A]].toIO
    )
  }
}
