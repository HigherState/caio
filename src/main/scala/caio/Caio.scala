package caio

import caio.std.{
  CaioApplicative,
  CaioAsync,
  CaioClock,
  CaioConcurrent,
  CaioFunctor,
  CaioMonad,
  CaioMonadCancel,
  CaioSpawn,
  CaioTemporal
}
import cats.{Align, Functor, Monoid, Parallel, SemigroupK, Traverse}
import cats.data.Ior
import cats.effect.{Async, Deferred, IO, Ref, Resource, Unique}
import cats.effect.kernel.{ParallelF, Poll}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

sealed trait Caio[-C, +L, +A] {
  final def <>[C1 <: C, L1 >: L, A1 >: A](that: => Caio[C1, L1, A1]): Caio[C1, L1, A1] =
    orElse(that)

  final def <*[C1 <: C, L1 >: L](fb: => Caio[C1, L1, Any]): Caio[C1, L1, A] =
    flatMap[C1, L1, A](a => fb.as(a))

  final def <*>[C1 <: C, L1 >: L, B](fb: => Caio[C1, L1, B]): Caio[C1, L1, (A, B)] =
    flatMap[C1, L1, (A, B)](a => fb.map(b => (a, b)))

  final def *>[C1 <: C, L1 >: L, B](fb: => Caio[C1, L1, B]): Caio[C1, L1, B] =
    flatMap[C1, L1, B](_ => fb)

  final def <&[C1 <: C, L1 >: L](that: Caio[C1, L1, Any]): Caio[C1, L1, A]        =
    both(that).map { case (a, _) => a }

  final def <&>[C1 <: C, L1 >: L, B](that: Caio[C1, L1, B]): Caio[C1, L1, (A, B)] =
    both(that)

  final def &>[C1 <: C, L1 >: L, B](that: Caio[C1, L1, B]): Caio[C1, L1, B]    =
    both(that).map { case (_, b) => b }

  final def >>=[C1 <: C, L1 >: L, B](f: A => Caio[C1, L1, B]): Caio[C1, L1, B] =
    flatMap(f)

  final def <+>[C1 <: C, L1 >: L, B](that: => Caio[C1, L1, B]): Caio[C1, L1, Either[A, B]] =
    orElseEither(that)

  final def <|>[C1 <: C, L1 >: L, B](that: Caio[C1, L1, B]): Caio[C1, L1, Either[A, B]] =
    race(that)

  final def andWait(duration: FiniteDuration): Caio[C, L, A] =
    this <* Caio.sleep(duration)

  final def as[B](b: => B): Caio[C, L, B] =
    map(_ => b)

  final def attempt: Caio[C, L, Either[Throwable, A]] =
    map(Right.apply).handleErrorWith(ex => Caio.pure(Left(ex)))

  final def background: Resource[Caio[C @uncheckedVariance, L @uncheckedVariance, *] @uncheckedVariance, Caio[
    C,
    L,
    OutcomeCaio[C @uncheckedVariance, L @uncheckedVariance, A @uncheckedVariance]
  ]] =
    CaioSpawn[C, L].background(this)

  final def bracket[C1 <: C, L1 >: L, A1 >: A, B](use: A1 => Caio[C1, L1, B])(
    release: A1 => Caio[C1, L1, Unit]
  ): Caio[C1, L1, B] =
    bracketCase(use) { case (a, _) => release(a) }

  final def bracketCase[C1 <: C, L1 >: L, A1 >: A, B](use: A1 => Caio[C1, L1, B])(
    release: (A1, OutcomeCaio[C1, L1, B]) => Caio[C1, L1, Unit]
  ): Caio[C1, L1, B] =
    CaioMonadCancel[C1, L1].bracketCase(this)(use)(release)

  final def both[C1 <: C, L1 >: L, B](other: => Caio[C1, L1, B]): Caio[C1, L1, (A, B)] =
    CaioAsync[C1, L1].both(this, other)

  final def censor[L1 >: L](f: L1 => L1)(implicit M: Monoid[L1]): Caio[C, L1, A] =
    Caio.CensorCaio(this, f, () => M.empty)

  final def censorWithDefault[L1 >: L](default: => L1)(f: L1 => L1): Caio[C, L1, A] =
    Caio.CensorCaio(this, f, () => default)

  final def clear: Caio[C, Nothing, A] =
    Caio.ClearCaio(this)

  final def contramap[C0](f: C0 => C): Caio[C0, L, A] =
    Caio.getContext[C0].flatMap(c0 => provideContext(f(c0)))

  final def delayBy(duration: FiniteDuration): Caio[C, L, A] =
    Caio.sleep(duration) *> this

  final def flatMap[C1 <: C, L1 >: L, B](f: A => Caio[C1, L1, B]): Caio[C1, L1, B] =
    Caio.BindCaio(this, f)

  final def flatten[C1 <: C, L1 >: L, B](implicit ev: A <:< Caio[C1, L1, B]): Caio[C1, L1, B] =
    flatMap[C1, L1, B](ev)

  final def flatTap[C1 <: C, L1 >: L](f: A => Caio[C1, L1, Any]): Caio[C1, L1, A] =
    flatMap[C1, L1, A](a => f(a).as(a))

  final def forceR[C1 <: C, L1 >: L, B](other: Caio[C1, L1, B]): Caio[C1, L1, B] =
    attempt *> other

  final def foreverM: Caio[C, L, Nothing] =
    CaioMonad[C, L].foreverM[A, Nothing](this)

  final def guarantee[C1 <: C, L1 >: L, A1 >: A](finalizer: Caio[C1, L1, Unit]): Caio[C1, L1, A1] =
    guaranteeCase[C1, L1, A1](_ => finalizer)

  final def guaranteeCase[C1 <: C, L1 >: L, A1 >: A](
    finalizer: OutcomeCaio[C1, L1, A1] => Caio[C1, L1, Unit]
  ): Caio[C1, L1, A1] =
    CaioMonadCancel[C1, L1].guaranteeCase[A1](this)(finalizer)

  final def head[B](implicit ev: A <:< List[B]): Caio[C, L, Option[B]] =
    map(a => ev(a).headOption)

  final def handleError[A2 >: A](f: Throwable => A2): Caio[C, L, A2] =
    handleErrorWith[C, L, A2](t => Caio.pure(f(t)))

  final def handleErrorWith[C1 <: C, L1 >: L, A2 >: A](f: Throwable => Caio[C1, L1, A2]): Caio[C1, L1, A2] =
    Caio.HandleErrorCaio[C1, L1, A2](this, { case ex: Throwable => f(ex) })

  final def ifM[C1 <: C, L1 >: L, B](ifTrue: => Caio[C1, L1, B], ifFalse: => Caio[C1, L1, B])(implicit
    ev: A <:< Boolean
  ): Caio[C1, L1, B]                                                                                       =
    flatMap(a => if (ev(a)) ifTrue else ifFalse)

  final def iterateUntil(p: A => Boolean): Caio[C, L, A] =
    CaioMonad[C, L].iterateUntil(this)(p)

  final def iterateWhile(p: A => Boolean): Caio[C, L, A] =
    CaioMonad[C, L].iterateWhile(this)(p)

  final def listen(implicit M: Monoid[L @uncheckedVariance]): Caio[C, L, (A, L)] =
    Caio.ListenCaio[C, L, A](this, () => M.empty)

  final def listenWithDefault[L1 >: L](default: => L1): Caio[C, L1, (A, L1)] =
    Caio.ListenCaio[C, L1, A](this, () => default)

  final def localContext[C1 <: C](f: C1 => C1): Caio[C1, L, A] =
    Caio.LocalContextCaio(this, f)

  final def map[B](f: A => B): Caio[C, L, B] =
    Caio.BindCaio[C, L, A, B](this, a => Caio.PureCaio(f(a)))

  final def memoize: Caio[C, L, Caio[C, L, A]] =
    CaioConcurrent[C, L].memoize(this)

  final def onCancel[C1 <: C, L1 >: L](finalizer: Caio[C1, L1, Unit]): Caio[C1, L1, A] =
    Caio.KleisliCaio[C1, L1, A] { case (c, ref) =>
      Caio.foldIO(this, c, ref).onCancel(Caio.run(finalizer, c, ref))
    }

  final def onError[C1 <: C, L1 >: L](f: Throwable => Caio[C1, L1, Any]): Caio[C1, L1, A] =
    handleErrorWith(t => f(t).attempt *> Caio.raiseError(t))

  final def option: Caio[C, L, Option[A]] =
    redeem(_ => None, Some(_))

  final def orElse[C1 <: C, L1 >: L, A1 >: A](that: => Caio[C1, L1, A1]): Caio[C1, L1, A1] =
    handleErrorWith[C1, L1, A1](_ => that)

  final def orElseEither[C1 <: C, L1 >: L, B](that: => Caio[C1, L1, B]): Caio[C1, L1, Either[A, B]] =
    map(Left.apply).orElse(that.map(Right.apply))

  final def provideContext(c: C): Caio[Any, L, A] =
    Caio.RefIOCaio[L, A](ref => Caio.run[C, L, A](this, c, ref))

  final def race[C1 <: C, L1 >: L, B](that: Caio[C1, L1, B]): Caio[C1, L1, Either[A, B]] =
    CaioSpawn[C1, L1].race(this, that)

  final def racePair[C1 <: C, L1 >: L, B](that: Caio[C1, L1, B]): Caio[C1, L1, Either[
    (OutcomeCaio[C1, L1, A @uncheckedVariance], FiberCaio[C1, L1, B]),
    (FiberCaio[C1, L1, A @uncheckedVariance], OutcomeCaio[C1, L1, B])
  ]] =
    CaioSpawn[C1, L1].racePair[A @uncheckedVariance, B](this, that)

  final def recover[A2 >: A](f: PartialFunction[Throwable, A2]): Caio[C, L, A2] =
    recoverWith[C, L, A2](t => Caio.pure(f(t)))

  final def recoverWith[C1 <: C, L1 >: L, A1 >: A](f: PartialFunction[Throwable, Caio[C1, L1, A1]]): Caio[C1, L1, A1] =
    Caio.HandleErrorCaio(this, f)

  final def redeem[B](recover: Throwable => B, map: A => B): Caio[C, L, B] =
    attempt.map(_.fold(recover, map))

  final def redeemWith[C1 <: C, L1 >: L, B](
    recover: Throwable => Caio[C1, L1, B],
    bind: A => Caio[C1, L1, B]
  ): Caio[C1, L1, B] =
    attempt.flatMap(_.fold(recover, bind))

  final def rethrow[B](implicit ev: A <:< Either[Throwable, B]): Caio[C, L, B] =
    flatMap(a => Caio.fromEither(ev(a)))

  final def replicateA(n: Int): Caio[C, L, List[A]] =
    CaioApplicative[C, L].replicateA(n, this)

  final def replicateA_(n: Int): Caio[C, L, Unit] =
    if (n <= 0) Caio.unit else this *> replicateA_(n - 1)

  final def start: Caio[C, L, FiberCaio[C @uncheckedVariance, L @uncheckedVariance, A @uncheckedVariance]] =
    CaioSpawn[C, L].start[A](this)

  final def timeoutTo[C1 <: C, L1 >: L, A1 >: A](duration: FiniteDuration, fallback: Caio[C1, L1, A1]): Caio[C1, L1, A1] =
    CaioTemporal[C1, L1].timeoutTo(this, duration, fallback)

  final def timeout(duration: FiniteDuration): Caio[C, L, A] =
    CaioTemporal[C, L].timeout(this, duration)

  final def timed: Caio[C, L, (FiniteDuration, A)] =
    CaioClock[C, L].timed(this)

  final def uncancelable: Caio[C, L, A] =
    Caio.uncancelable[C, L, A](_ => this)

  final def unit: Caio[C, L, Unit] =
    as(())

  final def void: Caio[C, L, Unit] =
    as(())

  final def run(c: C): IO[A] =
    Caio.run[C, L, A](this, c, Ref.unsafe[IO, Option[L]](None))

  final def run(implicit ev: C @uncheckedVariance =:= Any): IO[A] =
    Caio.run[Any, L, A](this.asInstanceOf[Caio[Any, L, A]], (), Ref.unsafe[IO, Option[L]](None))

  final def runContext(c: C): IO[(C @uncheckedVariance, Option[L], Either[Throwable, A])] =
    Caio.foldIO[C, L, A](this, c, Ref.unsafe[IO, Option[L]](None)).map {
      case FoldCaioSuccess(cOut, l, a) =>
        (cOut, l, Right(a))
      case FoldCaioError(cOut, l, ex)  =>
        (cOut, l, Left(ex))
    }
}

object Caio {
  type Par[C, L, A] = ParallelF[Caio[C, L, *], A]

  def apply[A](thunk: => A): Caio[Any, Nothing, A] =
    IOCaio[A](IO.apply(thunk))

  def async[C, L, A](k: (Either[Throwable, A] => Unit) => Caio[C, L, Option[Caio[C, L, Unit]]]): Caio[C, L, A] =
    CaioAsync[C, L].async[A](k)

  def async_[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[Any, Nothing, A] =
    CaioAsync[Any, Nothing].async_[A](k)

  def blocking[A](thunk: => A): Caio[Any, Nothing, A] =
    IOCaio[A](IO.blocking(thunk))

  def bracketFull[C, L, A, B](acquire: Poll[Caio[C, L, *]] => Caio[C, L, A])(use: A => Caio[C, L, B])(
    release: (A, OutcomeCaio[C, L, B]) => Caio[C, L, Unit]
  ): Caio[C, L, B] =
    CaioMonadCancel[C, L].bracketFull(acquire)(use)(release)

  def both[C, L, A, B](`this`: Caio[C, L, A], that: Caio[C, L, B]): Caio[C, L, (A, B)] =
    `this`.both(that)

  def canceled: Caio[Any, Nothing, Unit] =
    liftIO(IO.canceled)

  def cede: Caio[Any, Nothing, Unit] =
    liftIO(IO.cede)

  def defer[C, L, A](thunk: => Caio[C, L, A]): Caio[C, L, A] =
    delay(thunk).flatten

  def deferred[C, L, A]: Caio[C, Nothing, Deferred[Caio[C, L, *], A]] =
    delay(Deferred.unsafe[Caio[C, L, *], A])

  def delay[A](thunk: => A): Caio[Any, Nothing, A] =
    apply(thunk)

  def executionContext: Caio[Any, Nothing, ExecutionContext] =
    liftIO(IO.executionContext)

  def fromEither[A](either: => Either[Throwable, A]): Caio[Any, Nothing, A] =
    IOCaio(IO.fromEither(either))

  def fromFuture[C, L, A](caio: Caio[C, L, Future[A]]): Caio[C, L, A] =
    caio.flatMap(future => IOCaio(IO.fromFuture(IO(future))))

  def fromIOFuture[A](iof: IO[Future[A]]): Caio[Any, Nothing, A] =
    IOCaio(IO.fromFuture(iof))

  def fromTry[A](`try`: Try[A]): Caio[Any, Nothing, A] =
    IOCaio(IO.fromTry(`try`))

  def getContext[C]: Caio[C, Nothing, C] =
    GetContextCaio()

  def liftIO[A](io: => IO[A]): Caio[Any, Nothing, A] =
    IOCaio(io)

  def modifyContext[C](f: C => C): Caio[C, Nothing, Unit] =
    getContext[C].flatMap(c => setContext(f(c)))

  def monotonic: Caio[Any, Nothing, FiniteDuration] =
    liftIO(IO.monotonic)

  def never[A]: Caio[Any, Nothing, A] =
    IOCaio(IO.never[A])

  def none[A]: Caio[Any, Nothing, Option[A]] =
    pure(None)

  def parTraverseN[C, L, T[_]: Traverse, A, B](n: Int)(ta: T[A])(f: A => Caio[C, L, B]): Caio[C, L, T[B]] =
    asyncForCaio[C, L].parTraverseN(n)(ta)(f)

  def parSequenceN[C, L, T[_]: Traverse, A](n: Int)(tma: T[Caio[C, L, A]]): Caio[C, L, T[A]] =
    asyncForCaio[C, L].parSequenceN(n)(tma)

  def pure[A](a: A): Caio[Any, Nothing, A] =
    PureCaio(a)

  def raiseError(ex: Throwable): Caio[Any, Nothing, Nothing] =
    ErrorCaio(ex)

  def raiseUnless(cond: Boolean)(e: => Throwable): Caio[Any, Nothing, Unit] =
    Caio.unlessA(cond)(Caio.raiseError(e))

  def raiseWhen(cond: Boolean)(e: => Throwable): Caio[Any, Nothing, Unit] =
    Caio.whenA(cond)(Caio.raiseError(e))

  def race[C, L, A, B](fa: Caio[C, L, A], fb: Caio[C, L, B]): Caio[C, L, Either[A, B]] =
    fa.race(fb)

  def realTime: Caio[Any, Nothing, FiniteDuration] =
    liftIO(IO.realTime)

  def ref[C, L, A](a: A): Caio[Any, Nothing, Ref[Caio[C, L, *], A]] =
    delay(Ref.unsafe[Caio[C, L, *], A](a))

  def setContext[C](context: C): Caio[C, Nothing, Unit] =
    SetContextCaio(context)

  def sequence[C, L, T[_]: Traverse, A](tma: T[Caio[C, L, A]]): Caio[C, L, T[A]] =
    Traverse[T].sequence(tma)

  def sleep(duration: FiniteDuration): Caio[Any, Nothing, Unit] =
    CaioTemporal[Any, Nothing].sleep(duration)

  def some[A](a: A): Caio[Any, Nothing, Option[A]] =
    pure(Some(a))

  def tell[L: Monoid](l: L): Caio[Any, L, Unit] =
    TellCaio[L](l)

  def traverse[C, L, T[_]: Traverse, A, B](ta: T[A])(f: A => Caio[C, L, B]): Caio[C, L, T[B]] =
    Traverse[T].traverse(ta)(f)

  def uncancelable[C, L, A](body: Poll[Caio[C, L, *]] => Caio[C, L, A]): Caio[C, L, A] =
    Caio.KleisliCaio[C, L, A] { (c, ref) =>
      IO.uncancelable { poll =>
        val newPoll = new Poll[Caio[C, L, *]] {
          def apply[X](fa: Caio[C, L, X]): Caio[C, L, X] =
            KleisliCaio[C, L, X] { case (cc, _) =>
              poll(foldIO(fa, cc, ref))
            }
        }

        foldIO[C, L, A](body(newPoll), c, ref)
      }
    }

  def unit: Caio[Any, Nothing, Unit] =
    pure(())

  def unique: Caio[Any, Nothing, Unique.Token] =
    liftIO(IO.unique)

  def unlessA[C, L](cond: Boolean)(action: => Caio[C, L, Unit]): Caio[C, L, Unit] =
    CaioApplicative[C, L].unlessA(cond)(action)

  def whenA[C, L](cond: Boolean)(action: => Caio[C, L, Unit]): Caio[C, L, Unit] =
    CaioApplicative[C, L].whenA(cond)(action)

  implicit def alignForCaio[C, L]: Align[Caio[C, L, *]] =
    new Align[Caio[C, L, *]] {
      val functor: Functor[Caio[C, L, *]] =
        CaioFunctor[C, L]

      def align[A, B](fa: Caio[C, L, A], fb: Caio[C, L, B]): Caio[C, L, Ior[A, B]] =
        fa.redeemWith(
          t => fb.redeemWith(_ => raiseError(t), b => pure(Ior.right(b))),
          a => fb.redeem(_ => Ior.left(a), b => Ior.both(a, b))
        )
    }

  implicit def asyncForCaio[C, L]: Async[Caio[C, L, *]] =
    CaioAsync[C, L]

  implicit def monoidForCaio[C, L, A](implicit M: Monoid[A]): Monoid[Caio[C, L, A]] =
    new Monoid[Caio[C, L, A]] {
      def combine(left: Caio[C, L, A], right: Caio[C, L, A]): Caio[C, L, A] =
        left.flatMap(l => right.map(r => M.combine(l, r)))

      def empty: Caio[C, L, A] =
        Caio.pure(M.empty)
    }

  implicit def parallelForCaio[C, L]: Parallel.Aux[Caio[C, L, *], Par[C, L, *]] =
    cats.effect.instances.spawn.parallelForGenSpawn[Caio[C, L, *], Throwable]

  implicit def semigroupKForIO[C, L]: SemigroupK[Caio[C, L, *]] =
    new SemigroupK[Caio[C, L, *]] {
      final override def combineK[A](a: Caio[C, L, A], b: Caio[C, L, A]): Caio[C, L, A] =
        a.handleErrorWith(_ => b)
    }

  @inline private[caio] def run[C, L, A](caio: Caio[C, L, A], c: C, ref: Ref[IO, Option[L]]): IO[A] =
    foldIO[C, L, A](caio, c, ref).flatMap(_.toIO)

  private type Continuation  = (Any, Any) => Caio[Any, Any, Any]
  private type ErrorRecovery = (Any, Throwable) => Caio[Any, Any, Any]

  sealed private trait Handler
  final private case class OnSuccess(f: Continuation) extends Handler
  final private case class OnError(f: ErrorRecovery)  extends Handler

  @inline private def tryOrError(value: => Caio[Any, Any, Any]): Caio[Any, Any, Any]                 =
    try value
    catch { case NonFatal(ex) => ErrorCaio(ex) }

  @inline @tailrec private def nextHandler(fs: List[Handler]): Option[(Continuation, List[Handler])] =
    fs match {
      case OnSuccess(c) :: cs => Some((c, cs))
      case _ :: cs            => nextHandler(cs)
      case Nil                => None
    }

  @inline @tailrec private def nextErrorHandler(fs: List[Handler]): Option[(ErrorRecovery, List[Handler])] =
    fs match {
      case OnError(c) :: cs => Some((c, cs))
      case _ :: cs          => nextErrorHandler(cs)
      case Nil              => None
    }

  @inline private[caio] def foldIO[C, L, A](caio: Caio[C, L, A], c: C, ref: Ref[IO, Option[L]]): IO[FoldCaioPure[C, L, A]] = {
    def safeFold(caio: Caio[Any, Any, Any], c: Any, handlers: List[Handler]): IO[FoldCaioPure[Any, Any, Any]] = {
      @tailrec def foldCaio(caio: Caio[Any, Any, Any], c: Any, handlers: List[Handler]): IO[FoldCaioPure[Any, Any, Any]] =
        caio match {
          case BindCaio(PureCaio(a), f) =>
            foldCaio(tryOrError(f(a)), c, handlers)

          case BindCaio(caio: IOCaio[_], f) =>
            caio.f().redeemWith(e => safeFold(ErrorCaio(e), c, handlers), a => safeFold(tryOrError(f(a)), c, handlers))

          case BindCaio(source, f) =>
            foldCaio(source, c, OnSuccess((_, a) => f(a)) :: handlers)

          case PureCaio(a) =>
            nextHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, a)), c, fs)
              case None          =>
                ref.get.map(l => FoldCaioSuccess(c, l, a))
            }

          case caio: IOCaio[_] =>
            //The IO monad will bring this back into stack safety
            caio.f().redeemWith(e => safeFold(ErrorCaio(e), c, handlers), a => safeFold(PureCaio(a), c, handlers))

          case RefIOCaio(f) =>
            //The IO monad will bring this back into stack safety
            f(ref.asInstanceOf[Ref[IO, Option[Any]]])
              .redeemWith(e => safeFold(ErrorCaio(e), c, handlers), a => safeFold(PureCaio(a), c, handlers))

          case KleisliCaio(f) =>
            Try(f(c, ref.asInstanceOf[Ref[IO, Option[Any]]])) match {
              //Doesnt support Error or Failure handling
              case scala.util.Success(foldIO) =>
                foldIO
                  .flatMap {
                    case FoldCaioSuccess(c, _, a) =>
                      //The IO monad will bring this back into stack safety
                      safeFold(PureCaio(a), c, handlers)
                    case FoldCaioError(c, _, ex)  =>
                      safeFold(ErrorCaio(ex), c, handlers)
                  }
                  .handleErrorWith(ex => safeFold(ErrorCaio(ex), c, handlers))
              case scala.util.Failure(ex)     =>
                foldCaio(ErrorCaio(ex), c, handlers)
            }

          case ErrorCaio(e) =>
            nextErrorHandler(handlers) match {
              case Some((f, fs)) =>
                foldCaio(tryOrError(f(c, e)), c, fs)
              case None          =>
                ref.get.map(l => FoldCaioError(c, l, e))
            }

          case HandleErrorCaio(source, f) =>
            foldCaio(source, c, OnError((_, e) => if (f.isDefinedAt(e)) f(e) else ErrorCaio(e)) :: handlers)

          case tellCaio @ TellCaio(l2) =>
            foldCaio(
              IOCaio(
                ref.update(opt => Some(tellCaio.monoid.combine(opt.getOrElse(tellCaio.monoid.empty), l2).asInstanceOf[L]))
              ),
              c,
              handlers
            )

          case ListenCaio(source, default) =>
            foldCaio(
              source,
              c,
              OnSuccess((_, a) => IOCaio(ref.get.map(opt => a -> opt.getOrElse(default())))) :: handlers
            )

          case CensorCaio(source, f, default) =>
            foldCaio(
              source,
              c,
              OnSuccess((_, a) =>
                BindCaio(
                  IOCaio(ref.update(opt => Some(f(opt.getOrElse(default())).asInstanceOf[L]))),
                  (_: Any) => PureCaio(a)
                )
              ) :: handlers
            )

          case ClearCaio(source) =>
            foldCaio(source, c, OnSuccess((_, a) => IOCaio(ref.set(None).as(a))) :: handlers)

          case GetContextCaio() =>
            foldCaio(PureCaio(c), c, handlers)

          case SetContextCaio(replaceC) =>
            foldCaio(Caio.unit, replaceC, handlers)

          case LocalContextCaio(fa, f) =>
            foldCaio(
              fa,
              f(c),
              OnSuccess((_, a) => BindCaio(SetContextCaio(c), (_: Unit) => PureCaio(a))) ::
                OnError((_, e) => BindCaio(SetContextCaio(c), (_: Unit) => ErrorCaio(e))) ::
                handlers
            )
        }

      foldCaio(caio, c, handlers)
    }

    IO.defer(safeFold(caio.asInstanceOf[Caio[Any, Any, Any]], c, Nil)).asInstanceOf[IO[FoldCaioPure[C, L, A]]]
  }

  final private case class PureCaio[+A](a: A) extends Caio[Any, Nothing, A]

  final private class IOCaio[+A] private (val f: () => IO[A]) extends Caio[Any, Nothing, A]

  private object IOCaio {
    def apply[A](a: => IO[A])                          = new IOCaio(() => a)
    def unapply[A](io: IOCaio[A]): Option[() => IO[A]] = Some(io.f)
  }

  final private[caio] case class RefIOCaio[L, +A](f: Ref[IO, Option[L]] => IO[A]) extends Caio[Any, L, A]

  final private[caio] case class KleisliCaio[C, L, +A](kleisli: (C, Ref[IO, Option[L]]) => IO[FoldCaioPure[C, L, A]])
      extends Caio[C, L, A]

  final private case class BindCaio[-C, +L, E, +A](source: Caio[C, L, E], f: E => Caio[C, L, A]) extends Caio[C, L, A]

  final private case class ErrorCaio(e: Throwable) extends Caio[Any, Nothing, Nothing]

  final private case class HandleErrorCaio[-C, +L, +A](source: Caio[C, L, A], f: PartialFunction[Throwable, Caio[C, L, A]])
      extends Caio[C, L, A]

  final private case class TellCaio[L](l: L)(implicit val monoid: Monoid[L]) extends Caio[Any, L, Unit]

  final private case class ListenCaio[-C, +L, +A](source: Caio[C, L, A], default: () => L) extends Caio[C, L, (A, L)]

  final private case class CensorCaio[-C, L, +A](source: Caio[C, L, A], f: L => L, default: () => L)
      extends Caio[C, L, A]

  final private case class ClearCaio[-C, L, +A](source: Caio[C, L, A]) extends Caio[C, Nothing, A]

  final private case class GetContextCaio[C]() extends Caio[C, Nothing, C]

  final private case class SetContextCaio[C](c: C) extends Caio[C, Nothing, Unit]

  final private case class LocalContextCaio[C, L, A](source: Caio[C, L, A], f: C => C) extends Caio[C, L, A]
}
