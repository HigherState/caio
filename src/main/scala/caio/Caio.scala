package caio

import cats.effect.IO

import scala.util.control.NonFatal


sealed trait Caio[A] {

  def flatMap[B](f: A => Caio[B]): Caio[B]

  def map[B](f: A => B): Caio[B]

  private[caio] def combineState(proceedingState:State):Caio[A]

  private[caio] def toResult(evalContext:Context):Result[A]

  //Can operate on error cases
  private[caio] def mapState(f: State => State): Caio[A]

  private[caio] def mapWithState[B](f: (A, State) => (B, State)): Caio[B]

  def runAsync():IO[(Either[Throwable, A], EventLog)]
}


private[caio] sealed trait Result[A] {
  def map[B](f: A => B): Result[B]
  def mapState[B](f:State => State):Result[A]
  def mapWithState[B](f: (A, State) => (B, State)): Result[B]
  def flatMapC[B](context:Context)(f: A => Caio[B]): Result[B]
  def combineState(proceedingState:State):Result[A]
  def toIO: IO[PureResult[A]]

}

private[caio] sealed trait PureResult[A] extends Result[A] {
  def map[B](f: A => B): PureResult[B]
  def mapState[B](f:State => State):PureResult[A]
  def mapWithState[B](f: (A, State) => (B, State)): PureResult[B]
  def combineState(proceedingState:State):PureResult[A]
  def state:State
}


private[caio] final case class SuccessResult[A] private(a:A, state:State) extends PureResult[A] {
  def map[B](f: A => B): PureResult[B] =
    ResultOps.tryCatchPure(state)(SuccessResult(f(a), state))

  def combineState(proceedingState: State): PureResult[A] =
    SuccessResult(a, proceedingState + state)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](context:Context)(f: A => Caio[B]): Result[B] =
    ResultOps.tryCatch(state)(f(a).combineState(state).toResult(context))

  def mapState[B](f:State => State):PureResult[A] =
    ResultOps.tryCatchPure(state)(SuccessResult(a, state))

  def mapWithState[B](f: (A, State) => (B, State)): PureResult[B] =
    ResultOps.tryCatchPure(state){
      val (b, newState) = f(a, state)
      SuccessResult(b, newState)
    }
}

private[caio] final case class ErrorResult[A] private(e:Throwable, state:State) extends PureResult[A] {
  @inline
  private def shiftValue[B]: ErrorResult[B] =
    this.asInstanceOf[ErrorResult[B]]

  def map[B](f: A => B): ErrorResult[B] =
    shiftValue[B]

  def combineState(proceedingState: State): PureResult[A] =
    ErrorResult(e, proceedingState + state)

  def handleWith[B](f: Throwable => B): B = f(e)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](context:Context)(f: A => Caio[B]): Result[B] =
    this.asInstanceOf[Result[B]]

  def mapState[B](f:State => State):PureResult[A] = {
    ResultOps.tryCatchPure(state, e)(ErrorResult(e, state))
  }

  def mapWithState[B](f: (A, State) => (B, State)): PureResult[B] =
    shiftValue[B]

}

private[caio] final case class IOResult[A](io:IO[PureResult[A]]) extends Result[A] {

  def map[B](f: A => B): IOResult[B] =
    IOResult(io.map(_.map(f)))

  def combineState(proceedingState: State): Result[A] =
    IOResult(io.map(_.combineState(proceedingState)))

  def mapRes[B](f: Result[A] => Result[B]): IOResult[B] =
    IOResult(io.flatMap(a => f(a).toIO))

  def toIO: IO[PureResult[A]] = io

  def flatMapC[B](context:Context)(f: A => Caio[B]): Result[B] =
    IOResult(io.flatMap{ r =>
      r.flatMapC(context)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[B] =>
          IO.pure(pure)
      }
    })

  def mapState[B](f:State => State):Result[A] =
    IOResult(io.map(_.mapState(f)))

  def mapWithState[B](f: (A, State) => (B, State)): Result[B] =
    IOResult(io.map(_.mapWithState(f)))

}

private[caio] object ResultOps {

  @inline
  def tryCatchPure[A](state:State)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, state)
    }

  @inline
  def tryCatchPure[A](state:State, e:Throwable)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Throwables(e, ex), state)
    }

  @inline
  def tryCatch[A](state:State)(f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, state)
    }

  @inline
  def tryCatch[A](f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, State.empty)
    }
}

private[caio] final case class CaioPure[A](a:A) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(CaioPure(f(a)))

  def flatMap[B]( f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(f(a))

  def combineState(proceedingState: State): Caio[A] =
    CaioState(a, proceedingState)

  private[caio] def toResult(context:Context):Result[A] =
    SuccessResult(a, State.empty)

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> EventLog.empty)

  private[caio] def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(CaioState(a, f(State.empty)))

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioOps.tryCatch{
      val (b, newState) = f(a, State.empty)
      CaioState(b, newState)
    }

}

private[caio] final case class CaioError[A](e:Throwable, state:State) extends Caio[A] {

  @inline
  private def shiftValue[B]: CaioError[B] =
    this.asInstanceOf[CaioError[B]]

  def map[B](f: A => B):Caio[B] =
    shiftValue[B]

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    shiftValue[B]

  private[caio] def combineState(proceedingState: State) =
    CaioError(e, proceedingState + state)

  private[caio] def toResult(context:Context):Result[A] =
    ErrorResult(e, state)

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Left(e) -> State.getLog(state))

  private[caio] def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(state, e)(CaioError(e, f(state)))

  private[caio] def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    shiftValue[B]

}



private[caio] final case class CaioState[A](a:A, state:State) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(state)(CaioState(f(a), state))

  def flatMap[B](f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(state)(f(a).combineState(state))

  private[caio] def combineState(proceedingState: State) =
    CaioState(a, proceedingState + state)

  private[caio] def toResult(context:Context):Result[A] =
    SuccessResult(a, state)

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> State.getLog(state))

  private[caio] def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(state)(CaioState(a, f(state)))

  private[caio] def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioOps.tryCatch(state){
      val (b, newState) = f(a, state)
      CaioState(b, newState)
    }
}


private[caio] final case class CaioKleisli[A](func:Context => Result[A]) extends Caio[A] {

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    CaioKleisli { c =>
      ResultOps.tryCatch{
        func(c).flatMapC(c)(f)
      }
    }

  def map[B](f: A => B):Caio[B] =
    CaioKleisli { c => func(c).map(f) }

  private[caio] def combineState(proceedingState: State) =
    CaioKleisli(func.andThen(_.combineState(proceedingState)))

  private[caio] def toResult(context:Context):Result[A] =
    ResultOps.tryCatch{func(context)}

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(func(Context.empty).toIO.map{
      case ErrorResult(e, c) =>
        Left(e) -> State.getLog(c)
      case SuccessResult(a, c) =>
        Right(a) -> State.getLog(c)
    }).flatMap(identity)
    .handleErrorWith(e => IO.pure(Left(e) -> EventLog.empty))

  private[caio] def mapState(f: State => State): Caio[A] =
    CaioKleisli { c => func(c).mapState(f) }

  private[caio] def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioKleisli { c => func(c).mapWithState(f) }
}


private[caio] object CaioOps {

  @inline
  def tryCatch[A](f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(ex, State.empty)
    }

  @inline
  def tryCatch[A](state:State)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(ex, state)
    }

  @inline
  def tryCatch[A](state:State, e:Throwable)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(Throwables(ex, e), state)
    }
}

