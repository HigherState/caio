package caio

import cats.effect.IO

import scala.util.control.NonFatal


sealed trait Caio[A] {

  def flatMap[B](f: A => Caio[B]): Caio[B]

  def map[B](f: A => B): Caio[B]

  def combineState(proceedingStore:Store):Caio[A]

  def toResult(evalContext:Arguments):Result[A]

  def toIOResult(evalContext:Arguments):IOResult[A]

  //Can operate on error cases
  def mapStore(f: Store => Store): Caio[A]

  def mapWithStore[B](f: (A, Store) => (B, Store)): Caio[B]

  def handleError[B](f: Throwable => Caio[A]): Caio[A]

  def runAsync(arguments:Arguments = Arguments.empty):IO[(Either[Throwable, A], EventLog)]
}


private[caio] final case class CaioPure[A](a:A) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(CaioPure(f(a)))

  def flatMap[B]( f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(f(a))

  def combineState(proceedingStore: Store): Caio[A] =
    CaioState(a, proceedingStore)

  def toResult(arguments:Arguments):Result[A] =
    SuccessResult(a, Store.empty)

  def toIOResult(arguments:Arguments):IOResult[A] =
    IOResult(IO.pure(SuccessResult(a, Store.empty)))

  def runAsync(arguments:Arguments = Arguments.empty):IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> EventLog.empty)

  def mapStore(f: Store => Store): Caio[A] =
    CaioOps.tryCatch(CaioState(a, f(Store.empty)))

  def mapWithStore[B](f: (A, Store) => (B, Store)): Caio[B] =
    CaioOps.tryCatch{
      val (b, newStore) = f(a, Store.empty)
      CaioState(b, newStore)
    }

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    this

}

private[caio] final case class CaioError[A](e:Throwable, store:Store) extends Caio[A] {

  @inline
  private def shiftValue[B]: CaioError[B] =
    this.asInstanceOf[CaioError[B]]

  def map[B](f: A => B):Caio[B] =
    shiftValue[B]

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    shiftValue[B]

 def combineState(proceedingStore: Store) =
    CaioError(e, proceedingStore + store)

  def toResult(arguments:Arguments):Result[A] =
    ErrorResult(e, store)

  def toIOResult(arguments:Arguments):IOResult[A] =
    IOResult(IO.pure(ErrorResult(e, store)))

  def runAsync(arguments:Arguments = Arguments.empty):IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Left(e) -> Store.getLog(store))

  def mapStore(f: Store => Store): Caio[A] =
    CaioOps.tryCatch(store, e)(CaioError(e, f(store)))

  def mapWithStore[B](f: (A, Store) => (B, Store)): Caio[B] =
    shiftValue[B]

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    CaioOps.tryCatch(store, e)(f(e).combineState(store))

}



private[caio] final case class CaioState[A](a:A, store:Store) extends Caio[A] {

  def map[B](f: A => B): Caio[B] =
    CaioOps.tryCatch(store)(CaioState(f(a), store))

  def flatMap[B](f: A => Caio[B]): Caio[B] =
    CaioOps.tryCatch(store)(f(a).combineState(store))

  def combineState(proceedingStore: Store) =
    CaioState(a, proceedingStore + store)

  def toResult(arguments:Arguments):Result[A] =
    SuccessResult(a, store)

  def toIOResult(arguments:Arguments):IOResult[A] =
    IOResult(IO.pure(SuccessResult(a, store)))

  def runAsync(arguments:Arguments = Arguments.empty): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(Right(a) -> Store.getLog(store))

  def mapStore(f: Store => Store): Caio[A] =
    CaioOps.tryCatch(store)(CaioState(a, f(store)))

  def mapWithStore[B](f: (A, Store) => (B, Store)): Caio[B] =
    CaioOps.tryCatch(store){
      val (b, newStore) = f(a, store)
      CaioState(b, newStore)
    }

  def handleError[B](f: Throwable => Caio[A]): Caio[A] =
    this
}


private[caio] final case class CaioKleisli[A](func:Arguments => Result[A]) extends Caio[A] {

  def flatMap[B](f: A => Caio[B]):Caio[B] =
    CaioKleisli { c =>
      ResultOps.tryCatch{
        func(c).flatMapC(c)(f)
      }
    }

  def map[B](f: A => B):Caio[B] =
    CaioKleisli { c => func(c).map(f) }

  def combineState(proceedingStore: Store): Caio[A] =
    CaioKleisli{c => func(c.applyStore(proceedingStore)).combineStore(proceedingStore)}

  def toResult(arguments:Arguments):Result[A] =
    ResultOps.tryCatch{func(arguments)}

  def toIOResult(arguments:Arguments):IOResult[A] =
    IOResult(ResultOps.tryCatch{func(arguments)}.toIO)

  def runAsync(arguments:Arguments = Arguments.empty): IO[(Either[Throwable, A], EventLog)] =
    IO.delay(func(arguments).toIO.map{
      case ErrorResult(e, c) =>
        Left(e) -> Store.getLog(c)
      case SuccessResult(a, c) =>
        Right(a) -> Store.getLog(c)
    }).flatMap(identity)
    .handleErrorWith(e => IO.pure(Left(e) -> EventLog.empty))

  def mapStore(f: Store => Store): Caio[A] =
    CaioKleisli { c => func(c).mapStore(f) }

  def mapWithStore[B](f: (A, Store) => (B, Store)): Caio[B] =
    CaioKleisli { c => func(c).mapWithStore(f) }

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
        CaioError(ex, Store.empty)
    }

  @inline
  def tryCatch[A](store:Store)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(ex, store)
    }

  @inline
  def tryCatch[A](store:Store, e:Throwable)(f: => Caio[A]):Caio[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(Throwables.append(ex -> e), store)
    }
}


private[caio] sealed trait Result[A] {
  def map[B](f: A => B): Result[B]
  def mapStore[B](f:Store => Store):Result[A]
  def mapWithStore[B](f: (A, Store) => (B, Store)): Result[B]
  def flatMapC[B](arguments:Arguments)(f: A => Caio[B]): Result[B]
  def combineStore(proceedingStore:Store):Result[A]
  def toIO: IO[PureResult[A]]
  def handleError[B](arguments:Arguments)(f: Throwable => Caio[A]): Result[A]

}

private[caio] sealed trait PureResult[A] extends Result[A] {
  def map[B](f: A => B): PureResult[B]
  def mapStore[B](f:Store => Store):PureResult[A]
  def mapWithStore[B](f: (A, Store) => (B, Store)): PureResult[B]
  def combineStore(proceedingStore:Store):PureResult[A]
  def store:Store
}


private[caio] final case class SuccessResult[A] private(a:A, store:Store) extends PureResult[A] {
  def map[B](f: A => B): PureResult[B] =
    ResultOps.tryCatchPure(store)(SuccessResult(f(a), store))

  def combineStore(proceedingStore: Store): PureResult[A] =
    SuccessResult(a, proceedingStore + store)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](arguments:Arguments)(f: A => Caio[B]): Result[B] =
    ResultOps.tryCatch(store)(f(a).combineState(store).toResult(arguments))

  def mapStore[B](f:Store => Store):PureResult[A] =
    ResultOps.tryCatchPure(store)(SuccessResult(a, store))

  def mapWithStore[B](f: (A, Store) => (B, Store)): PureResult[B] =
    ResultOps.tryCatchPure(store){
      val (b, newStore) = f(a, store)
      SuccessResult(b, newStore)
    }

  def handleError[B](arguments:Arguments)(f: Throwable => Caio[A]): Result[A] =
    this
}

private[caio] final case class ErrorResult[A] private(e:Throwable, store:Store) extends PureResult[A] {
  @inline
  private def shiftValue[B]: ErrorResult[B] =
    this.asInstanceOf[ErrorResult[B]]

  def map[B](f: A => B): ErrorResult[B] =
    shiftValue[B]

  def combineStore(proceedingStore: Store): PureResult[A] =
    ErrorResult(e, proceedingStore + store)

  def handleWith[B](f: Throwable => B): B = f(e)

  def toIO: IO[PureResult[A]] = IO.delay(this)

  def flatMapC[B](arguments:Arguments)(f: A => Caio[B]): Result[B] =
    this.asInstanceOf[Result[B]]

  def mapStore[B](f:Store => Store):PureResult[A] = {
    ResultOps.tryCatchPure(store, e)(ErrorResult(e, store))
  }

  def mapWithStore[B](f: (A, Store) => (B, Store)): PureResult[B] =
    shiftValue[B]

  def handleError[B](arguments:Arguments)(f: Throwable => Caio[A]): Result[A] =
    ResultOps.tryCatch(store, e)(f(e).combineState(store).toResult(arguments))
}

private[caio] final case class IOResult[A](io:IO[PureResult[A]]) extends Result[A] {

  def map[B](f: A => B): IOResult[B] =
    IOResult(io.map(_.map(f)))

  def combineStore(proceedingStore: Store): Result[A] =
    IOResult(io.map(_.combineStore(proceedingStore)))

  def mapRes[B](f: Result[A] => Result[B]): IOResult[B] =
    IOResult(io.flatMap(a => f(a).toIO))

  def toIO: IO[PureResult[A]] = io

  def flatMapC[B](arguments:Arguments)(f: A => Caio[B]): Result[B] =
    IOResult(io.flatMap{ r =>
      r.flatMapC(arguments)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[B] =>
          IO.pure(pure)
      }
    })

  def handleError[B](arguments:Arguments)(f: Throwable => Caio[A]): Result[A] =
    IOResult(io.flatMap{ r =>
      r.handleError(arguments)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[A] =>
          IO.pure(pure)
      }
    })

  def mapStore[B](f:Store => Store):Result[A] =
    IOResult(io.map(_.mapStore(f)))

  def mapWithStore[B](f: (A, Store) => (B, Store)): Result[B] =
    IOResult(io.map(_.mapWithStore(f)))

}

private[caio] object ResultOps {

  def fromIO[A](ioa:IO[A]):IOResult[A] =
    IOResult(ioa.map(a => SuccessResult(a, Store.empty)).handleErrorWith(ex => IO(ErrorResult[A](ex, Store.empty))))

  @inline
  def tryCatchPure[A](store:Store)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, store)
    }

  @inline
  def tryCatchPure[A](store:Store, e:Throwable)(f: => PureResult[A]):PureResult[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Throwables.append(e -> ex), store)
    }

  @inline
  def tryCatch[A](store:Store)(f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, store)
    }

  @inline
  def tryCatch[A](store:Store, e:Throwable)(f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Throwables.append(e -> ex), store)
    }

  @inline
  def tryCatch[A](f: => Result[A]):Result[A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(ex, Store.empty)
    }

}

