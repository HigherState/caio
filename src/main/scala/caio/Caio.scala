package caio

import cats.effect.IO

import scala.util.control.NonFatal


sealed trait Caio[A] {

  def flatMap[B](f: A => Caio[B]): Caio[B]

  def map[B](f: A => B): Caio[B]

  def combineState(proceedingState:State):Caio[A]

  def toResult(evalContext:Context):Result[A]

  def toIOResult(evalContext:Context):IOResult[A]

  //Can operate on error cases
  def mapState(f: State => State): Caio[A]

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B]

  def handleError[B](f: Throwable => Caio[A]): Caio[A]

  def runAsync():IO[(Either[Throwable, A], EventLog)]
}


private[caio] final case class CaioPure[A](a:A) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(CaioPure(f(a)))

  def flatMap[B]( f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(f(a))

  def combineState(proceedingState: State): Caio[A] =
    CaioState(a, proceedingState)

  def toResult(context:Context):Result[A] =
    SuccessResult(a, State.empty)

  def toIOResult(context:Context):IOResult[A] =
    IOResult(IO.pure(SuccessResult(a, State.empty)))

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> EventLog.empty)

  def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(CaioState(a, f(State.empty)))

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioOps.tryCatch{
      val (b, newState) = f(a, State.empty)
      CaioState(b, newState)
    }

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    this

}

private[caio] final case class CaioError[A](e:Throwable, state:State) extends Caio[A] {

  @inline
  private def shiftValue[B]: CaioError[B] =
    this.asInstanceOf[CaioError[B]]

  def map[B](f: A => B):Caio[B] =
    shiftValue[B]

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    shiftValue[B]

 def combineState(proceedingState: State) =
    CaioError(e, proceedingState + state)

  def toResult(context:Context):Result[A] =
    ErrorResult(e, state)

  def toIOResult(context:Context):IOResult[A] =
    IOResult(IO.pure(ErrorResult(e, state)))

  def runAsync():IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Left(e) -> State.getLog(state))

  def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(state, e)(CaioError(e, f(state)))

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    shiftValue[B]

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    CaioOps.tryCatch(state, e)(f(e).combineState(state))

}



private[caio] final case class CaioState[A](a:A, state:State) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(state)(CaioState(f(a), state))

  def flatMap[B](f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(state)(f(a).combineState(state))

  def combineState(proceedingState: State) =
    CaioState(a, proceedingState + state)

  def toResult(context:Context):Result[A] =
    SuccessResult(a, state)

  def toIOResult(context:Context):IOResult[A] =
    IOResult(IO.pure(SuccessResult(a, state)))

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> State.getLog(state))

  def mapState(f: State => State): Caio[A] =
    CaioOps.tryCatch(state)(CaioState(a, f(state)))

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioOps.tryCatch(state){
      val (b, newState) = f(a, state)
      CaioState(b, newState)
    }

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    this
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

  def combineState(proceedingState: State) =
    CaioKleisli(func.andThen(_.combineState(proceedingState)))

  def toResult(context:Context):Result[A] =
    ResultOps.tryCatch{func(context)}

  def toIOResult(context:Context):IOResult[A] =
    IOResult(ResultOps.tryCatch{func(context)}.toIO)

  def runAsync(): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(func(Context.empty).toIO.map{
      case ErrorResult(e, c) =>
        Left(e) -> State.getLog(c)
      case SuccessResult(a, c) =>
        Right(a) -> State.getLog(c)
    }).flatMap(identity)
    .handleErrorWith(e => IO.pure(Left(e) -> EventLog.empty))

  def mapState(f: State => State): Caio[A] =
    CaioKleisli { c => func(c).mapState(f) }

  def mapWithState[B](f: (A, State) => (B, State)): Caio[B] =
    CaioKleisli { c => func(c).mapWithState(f) }

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    CaioKleisli { c => func(c).handleError(c)(f) }
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
        CaioError(Throwables.append(ex -> e), state)
    }
}


private[caio] sealed trait Result[A] {
  def map[B](f: A => B): Result[B]
  def mapState[B](f:State => State):Result[A]
  def mapWithState[B](f: (A, State) => (B, State)): Result[B]
  def flatMapC[B](context:Context)(f: A => Caio[B]): Result[B]
  def combineState(proceedingState:State):Result[A]
  def toIO: IO[PureResult[A]]
  def handleError[B](context:Context)(f: Throwable => Caio[A]): Result[A]

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

  def handleError[B](context:Context)(f: Throwable => Caio[A]): Result[A] =
    this
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

  def handleError[B](context:Context)(f: Throwable => Caio[A]): Result[A] =
    ResultOps.tryCatch(state, e)(f(e).combineState(state).toResult(context))
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

  def handleError[B](context:Context)(f: Throwable => Caio[A]): Result[A] =
    IOResult(io.flatMap{ r =>
      r.handleError(context)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[A] =>
          IO.pure(pure)
      }
    })

  def mapState[B](f:State => State):Result[A] =
    IOResult(io.map(_.mapState(f)))

  def mapWithState[B](f: (A, State) => (B, State)): Result[B] =
    IOResult(io.map(_.mapWithState(f)))

}

private[caio] object ResultOps {

  def fromIO[A](ioa:IO[A]):IOResult[A] =
    IOResult(ioa.map(a => SuccessResult(a, State.empty)).handleErrorWith(ex => IO(ErrorResult[A](ex, State.empty))))

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
        ErrorResult(Throwables.append(e -> ex), state)
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
  def tryCatch[A](state:State, e:Throwable)(f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Throwables.append(e -> ex), state)
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

