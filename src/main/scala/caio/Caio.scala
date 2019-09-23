package caio

import cats.effect.IO

import scala.util.control.NonFatal


sealed trait Caio[A] {

  def flatMap[B](f: A => Caio[B]): Caio[B]

  def map[B](f: A => B): Caio[B]

  private[caio] def combineContext(proceedingContext:Context):Caio[A]

  private[caio] def toResult(evalContext:Context):Result[A]

  private[Caio] def mapContext(f: Context => Context):Caio[A]

  def runAsync():IO[(Either[Throwable, A], EventLog)]
}


private[caio] sealed trait Result[A] {
  def map[B](f: A => B): Result[B]
  def mapC[B](proceedingContext:Context)(f: A => B): Result[B]
  def contextMap[B](f:Context => Context):Result[A]
  def flatMapC[B](proceedingContext:Context)(f: A => Caio[B]): Result[B]
  def combineContext(proceedingContext:Context):Result[A]
  def toIO: IO[PureResult[A]]

}

private[caio] sealed trait PureResult[A] extends Result[A] {
  def map[B](f: A => B): PureResult[B]
  def contextMap[B](f:Context => Context):PureResult[A]
  def mapC[B](proceedingContext:Context)(f: A => B): PureResult[B]
  def combineContext(proceedingContext:Context):PureResult[A]
  def context:Context
}


private[caio] final case class SuccessResult[A] private(a:A, context:Context) extends PureResult[A] {
  def map[B](f: A => B): PureResult[B] =
    ResultOps.tryCatchPure(context)(SuccessResult(f(a), context))

  def mapC[B](proceedingContext:Context)(f: A => B): PureResult[B] = {
    val newContext = proceedingContext + context
    ResultOps.tryCatchPure(newContext)(SuccessResult(f(a), newContext))
  }

  def combineContext(proceedingContext: Context): PureResult[A] =
    SuccessResult(a, proceedingContext + context)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](proceedingContext: Context)(f: A => Caio[B]): Result[B] =
    f(a).toResult(proceedingContext)

  def contextMap[B](f:Context => Context):PureResult[A] = {
    ResultOps.tryCatchPure(context)(SuccessResult(a, context))
  }

}

private[caio] final case class ErrorResult[A] private(e:Throwable, context:Context) extends PureResult[A] {
  @inline
  private def shiftValue[B]: ErrorResult[B] =
    this.asInstanceOf[ErrorResult[B]]

  def map[B](f: A => B): ErrorResult[B] =
    shiftValue[B]

  def mapC[B](proceedingContext:Context)(f: A => B): ErrorResult[B] =
    ErrorResult(e, proceedingContext + context)

  def combineContext(proceedingContext: Context): PureResult[A] =
    ErrorResult(e, proceedingContext + context)

  def handleWith[B](f: Throwable => B): B = f(e)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](proceedingContext: Context)(f: A => Caio[B]): Result[B] =
    ErrorResult(e, proceedingContext + context)

  def contextMap[B](f:Context => Context):PureResult[A] = {
    ResultOps.tryCatchPure(context, e)(ErrorResult(e, context))
  }

}

private[caio] final case class IOResult[A](io:IO[PureResult[A]]) extends Result[A] {
  import ResultOps._

  def mapC[B](proceedingContext:Context)(f: A => B): IOResult[B] =
    IOResult(io.map(_.mapC(proceedingContext)(f)))

  def map[B](f: A => B): IOResult[B] =
    IOResult(io.map(_.map(f)))

  def combineContext(proceedingContext: Context): Result[A] =
    IOResult(io.map(_.combineContext(proceedingContext)))

  def mapRes[B](f: Result[A] => Result[B]): IOResult[B] =
    IOResult(io.flatMap(a => f(a).toIO))

  def toIO: IO[PureResult[A]] = io

  def flatMapC[B](proceedingContext: Context)(f: A => Caio[B]): Result[B] =
    IOResult(io.flatMap{ r =>
      r.flatMapC(proceedingContext)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[B] =>
          IO.pure(pure)
      }
    })

  def contextMap[B](f:Context => Context):Result[A] = {
    IOResult(io.map(_.contextMap(f)))
  }

}

private[caio] object ResultOps {

  @inline
  def tryCatchPure[A](context:Context)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, context)
    }

  @inline
  def tryCatchPure[A](context:Context, e:Throwable)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Throwables(e, ex), context)
    }

  @inline
  def tryCatch[A](context:Context)(f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, context)
    }
}

private[caio] final case class CaioPure[A](a:A) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioPure(f(a))

  def flatMap[B]( f: A => Caio[B]): Caio[B] =
    f(a)

  def combineContext(proceedingContext: Context): Caio[A] =
    CaioContext(a, proceedingContext)

  private[caio] def toResult(proceedingContext:Context):Result[A] =
    SuccessResult(a, proceedingContext)

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> EventLog.empty)

  private[Caio] def mapContext(f: Context => Context): Caio[A] =
    CaioContext(a, f(Context.empty))
}

private[caio] final case class CaioError[A](e:Throwable, context:Context) extends Caio[A] {

  def map[B](f: A => B):Caio[B] =
    this.asInstanceOf[Caio[B]]

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    this.asInstanceOf[Caio[B]]

  private[caio] def combineContext(proceedingContext: Context) =
    CaioError(e, proceedingContext + context)

  private[caio] def toResult(proceedingContext:Context):Result[A] =
    ErrorResult(e, proceedingContext + context)

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Left(e) -> Context.getLog(context))

  private[Caio] def mapContext(f: Context => Context): Caio[A] =
    CaioOps.tryCatch(context, e)(CaioError(e, f(context)))
}



private[caio] final case class CaioContext[A](a:A, context:Context) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioPure(f(a))

  def flatMap[B](f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(context)(f(a).combineContext(context))

  private[caio] def combineContext(proceedingContext: Context) =
    CaioContext(a, proceedingContext + context)

  private[caio] def toResult(proceedingContext:Context):Result[A] =
    SuccessResult(a, proceedingContext + context)

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> Context.getLog(context))

  private[Caio] def mapContext(f: Context => Context): Caio[A] =
    f(context)
}


private[caio] final case class CaioKleisli[A](func:Context => Result[A]) extends Caio[A] {

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    CaioKleisli { c =>
      ResultOps.tryCatch(c){
        func(c).flatMapC(c)(f)
      }
    }

  def map[B](f: A => B):Caio[B] =
    CaioKleisli { c => func(c).map(f) }

  private[caio] def combineContext(proceedingContext: Context) =
    CaioKleisli(func.andThen(_.combineContext(proceedingContext)))

  private[caio] def toResult(evalContext:Context):Result[A] =
    func(evalContext)

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(func(Context.empty).toIO.map{
      case ErrorResult(e, c) =>
        Left(e) -> Context.getLog(c)
      case SuccessResult(a, c) =>
        Right(a) -> Context.getLog(c)
    }).flatMap(identity)
    .handleErrorWith(e => IO.pure(Left(e) -> EventLog.empty))
}


private[Caio] object CaioOps {

  @inline
  def tryCatch[A](context:Context)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(ex, context)
    }

  @inline
  def tryCatch[A](context:Context, e:Throwable)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(Throwables(ex, e), context)
    }
}

