package caio

import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.IO

import scala.util.control.NonFatal

sealed trait Caio[C, V, L, A] {

  def flatMap[B](f: A => Caio[C, V, L, B]): Caio[C, V, L, B]

  def map[B](f: A => B): Caio[C, V, L, B]


  def handleError[B](f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A]

  def handleFailures[B](f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A]


  def toResult(c:C):Result[C, V, L, A]

  def toIOResult(c:C):IOResult[C, V, L, A]

  def toIO(c:C):IO[(Either[ErrorOrFailure[V], A], C, L)]

  def unsafeRun(c:C):(Either[ErrorOrFailure[V], A], C, L) =
    toIO(c).unsafeRunSync()


  //Can operate on error cases
  private[caio] def mapStore(f: Store[C, L] => Store[C, L]): Caio[C, V, L, A]

  private[caio] def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Caio[C, V, L, B]

  private[caio] def combine(proceeding:Store[C, L]):Caio[C, V, L, A]
}

private[caio] final case class CaioState[C, V, L, A](a:A, store:Store[C, L]) extends Caio[C, V, L, A] {

  def map[B](f: A => B): Caio[C, V, L, B] =
    CaioOps.tryCatch(store)(CaioState(f(a), store))

  def flatMap[B](f: A => Caio[C, V, L, B]): Caio[C, V, L, B] =
    CaioOps.tryCatch(store)(f(a).combine(store))

  def combine(proceedingStore: Store[C, L]) =
    CaioState(a, proceedingStore + store)

  def toResult(c:C):Result[C, V, L, A] =
    SuccessResult(a, store)

  def toIOResult(c:C):IOResult[C, V, L, A] =
    IOResult(IO.pure(SuccessResult(a, store)))

  def toIO(c:C):IO[(Either[ErrorOrFailure[V], A], C, L)] =
    IO.delay((Right(a), store.getOrElse(c), store.l))

  def mapStore(f: Store[C, L] => Store[C, L]): Caio[C, V, L, A] =
    CaioOps.tryCatch(store)(CaioState(a, f(store)))

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Caio[C, V, L, B] =
    CaioOps.tryCatch(store){
      val (b, newStore) = f(a, store)
      CaioState(b, newStore)
    }

  def handleError[B](f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    this

  def handleFailures[B](f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A] =
    this
}

private[caio] final case class CaioError[C, V, L, A](e:ErrorOrFailure[V], store:Store[C, L]) extends Caio[C, V, L, A] {

  @inline
  private def shiftValue[B]: CaioError[C, V, L, B] =
    this.asInstanceOf[CaioError[C, V, L, B]]

  def map[B](f: A => B):Caio[C, V, L, B] =
    shiftValue[B]

  def flatMap[B](f: A => Caio[C, V, L, B]):Caio[C, V, L, B] =
    shiftValue[B]

  def combine(proceeding:Store[C, L]) =
    CaioError(e, proceeding + store)

  def toResult(c:C):Result[C, V, L, A] =
    ErrorResult(e, store)

  def toIOResult(c:C):IOResult[C, V, L, A] =
    IOResult(IO.pure(ErrorResult(e, store)))

  def toIO(c:C):IO[(Either[ErrorOrFailure[V], A], C, L)] =
    IO.delay((Left(e), store.getOrElse(c), store.l))

  def mapStore(f: Store[C, L] => Store[C, L]): Caio[C, V, L, A] =
    CaioOps.tryCatch(store)(CaioError(e, f(store)))

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Caio[C, V, L, B] =
    shiftValue[B]

  def handleError[B](f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    e.left.toOption.map{t =>
      CaioOps.tryCatch(store)(f(t).combine(store))
    }.getOrElse(this)

  def handleFailures[B](f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A] =
    e.right.toOption.map{v =>
      CaioOps.tryCatch(store)(f(v).combine(store))
    }.getOrElse(this)

}

private[caio] final case class CaioKleisli[C, V, L:Monoid, A](func:C => Result[C, V, L, A]) extends Caio[C, V, L, A] {

  def flatMap[B](f: A => Caio[C, V, L, B]):Caio[C, V, L, B] =
    CaioKleisli { c =>
      ResultOps.tryCatch(Store.empty[C, L]){
        func(c).flatMapC(c)(f)
      }
    }

  def map[B](f: A => B):Caio[C, V, L, B] =
    CaioKleisli { c => func(c).map(f) }

  def combine(proceedingStore: Store[C, L]): Caio[C, V, L, A] =
    CaioKleisli{c => func(proceedingStore.getOrElse(c)).combine(proceedingStore)}

  def toResult(c:C):Result[C, V, L, A] =
    ResultOps.tryCatch(Store.empty[C, L]){func(c)}

  def toIOResult(c:C):IOResult[C, V, L, A] =
    IOResult(ResultOps.tryCatch(Store.empty[C, L]){func(c)}.toIO)

  def toIO(c:C):IO[(Either[ErrorOrFailure[V], A], C, L)] =
    IO.delay(func(c).toIO.map{
      case ErrorResult(e, store) =>
        (Left(e), store.getOrElse(c), store.l)
      case SuccessResult(a, store) =>
        (Right(a), store.getOrElse(c), store.l)
    }).flatMap(identity)
    .handleErrorWith(e => IO.pure((Left(Left(e)), c, implicitly[Monoid[L]].empty)))

  def mapStore(f: Store[C, L] => Store[C, L]): Caio[C, V, L, A] =
    CaioKleisli { c => func(c).mapStore(f) }

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Caio[C, V, L, B] =
    CaioKleisli { c => func(c).mapWithStore(f) }

  def handleError[B](f: Throwable => Caio[C, V, L, A]): Caio[C, V, L, A] =
    CaioKleisli { c => func(c).handleError(c)(f) }

  def handleFailures[B](f: NonEmptyList[V] => Caio[C, V, L, A]): Caio[C, V, L, A] =
    CaioKleisli { c => func(c).handleFailures(c)(f) }
}


private[caio] object CaioOps {

  @inline
  def tryCatch[C, V, L, A](store:Store[C, L])(f: => Caio[C, V, L, A]):Caio[C, V, L, A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        CaioError(Left(ex), store)
    }
}


private[caio] sealed trait Result[C, V, L, A] {
  def map[B](f: A => B): Result[C, V, L, B]
  def mapStore[B](f:Store[C, L] => Store[C, L]):Result[C, V, L, A]
  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Result[C, V, L, B]
  def flatMapC[B](c:C)(f: A => Caio[C, V, L, B]): Result[C, V, L, B]
  def combine(proceedingStore:Store[C, L]):Result[C, V, L, A]
  def toIO: IO[PureResult[C, V, L, A]]
  def handleError[B](c:C)(f: Throwable => Caio[C, V, L, A]): Result[C, V, L, A]
  def handleFailures[B](c:C)(f: NonEmptyList[V] => Caio[C, V, L, A]): Result[C, V, L, A]

}

private[caio] sealed trait PureResult[C, V, L, A] extends Result[C, V, L, A] {
  def map[B](f: A => B): PureResult[C, V, L, B]
  def mapStore[B](f:Store[C, L] => Store[C, L]):PureResult[C, V, L, A]
  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): PureResult[C, V, L, B]
  def combine(proceedingStore:Store[C, L]):PureResult[C, V, L, A]
  def store:Store[C, L]
}


private[caio] final case class SuccessResult[C, V, L, A] private(a:A, store:Store[C, L]) extends PureResult[C, V, L, A] {
  def map[B](f: A => B): PureResult[C, V, L, B] =
    ResultOps.tryCatchPure(store)(SuccessResult(f(a), store))

  def combine(proceedingStore: Store[C, L]): PureResult[C, V, L, A] =
    SuccessResult(a, proceedingStore + store)

  def toIO: IO[PureResult[C, V, L, A]] =
    IO.delay(this)

  def flatMapC[B](c:C)(f: A => Caio[C, V, L, B]): Result[C, V, L, B] =
    ResultOps.tryCatch[C, V, L, B](store)(f(a).combine(store).toResult(store.getOrElse(c)))

  def mapStore[B](f:Store[C, L] => Store[C, L]):PureResult[C, V, L, A] =
    ResultOps.tryCatchPure[C, V, L, A](store)(SuccessResult(a, store))

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): PureResult[C, V, L, B] =
    ResultOps.tryCatchPure(store){
      val (b, newStore) = f(a, store)
      SuccessResult(b, newStore)
    }

  def handleError[B](c:C)(f: Throwable => Caio[C, V, L, A]): Result[C, V, L, A] =
    this

  def handleFailures[B](c: C)(f: NonEmptyList[V] => Caio[C, V, L, A]): Result[C, V, L, A] =
    this
}

private[caio] final case class ErrorResult[C, V, L, A](e:ErrorOrFailure[V], store:Store[C, L]) extends PureResult[C, V, L, A] {
  @inline
  private def shiftValue[B]: ErrorResult[C, V, L, B] =
    this.asInstanceOf[ErrorResult[C, V, L, B]]

  def map[B](f: A => B): ErrorResult[C, V, L, B] =
    shiftValue[B]

  def combine(proceedingStore: Store[C, L]): PureResult[C, V, L, A] =
    ErrorResult(e, proceedingStore + store)

  def toIO: IO[PureResult[C, V, L, A]] =
    IO.delay(this)

  def flatMapC[B](c:C)(f: A => Caio[C, V, L, B]): Result[C, V, L, B] =
    shiftValue[B]

  def mapStore[B](f:Store[C, L] => Store[C, L]):PureResult[C, V, L, A] =
    ResultOps.tryCatchPure(store)(ErrorResult(e, store))

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): PureResult[C, V, L, B] =
    shiftValue[B]

  def handleError[B](c:C)(f: Throwable => Caio[C, V, L, A]): Result[C, V, L, A] =
    e.left.toOption.map { t =>
      ResultOps.tryCatch(store)(f(t).combine(store).toResult(store.getOrElse(c)))
    }.getOrElse(this)


  def handleFailures[B](c: C)(f: NonEmptyList[V] => Caio[C, V, L, A]): Result[C, V, L, A] =
    e.right.toOption.map { v =>
      ResultOps.tryCatch(store)(f(v).combine(store).toResult(store.getOrElse(c)))
    }.getOrElse(this)
}

private[caio] final case class IOResult[C, V, L, A](io:IO[PureResult[C, V, L, A]]) extends Result[C, V, L, A] {

  def map[B](f: A => B): IOResult[C, V, L, B] =
    IOResult(io.map(_.map(f)))

  def combine(proceedingStore: Store[C, L]): Result[C, V, L, A] =
    IOResult(io.map(_.combine(proceedingStore)))

  def mapRes[B](f: Result[C, V, L, A] => Result[C, V, L, B]): IOResult[C, V, L, B] =
    IOResult(io.flatMap(a => f(a).toIO))

  def toIO: IO[PureResult[C, V, L, A]] = io

  def flatMapC[B](c:C)(f: A => Caio[C, V, L, B]): Result[C, V, L, B] =
    IOResult(io.flatMap{ r =>
      r.flatMapC(c)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[C, V, L, B] =>
          IO.pure(pure)
      }
    })

  def handleError[B](c:C)(f: Throwable => Caio[C, V, L, A]): Result[C, V, L, A] =
    IOResult(io.flatMap{ r =>
      r.handleError(c)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[C, V, L, A] =>
          IO.pure(pure)
      }
    })

  def handleFailures[B](c:C)(f: NonEmptyList[V] => Caio[C, V, L, A]): Result[C, V, L, A] =
    IOResult(io.flatMap{ r =>
      r.handleFailures(c)(f) match {
        case IOResult(io2) =>
          io2
        case pure:PureResult[C, V, L, A] =>
          IO.pure(pure)
      }
    })

  def mapStore[B](f:Store[C, L] => Store[C, L]):Result[C, V, L, A] =
    IOResult(io.map(_.mapStore(f)))

  def mapWithStore[B](f: (A, Store[C, L]) => (B, Store[C, L])): Result[C, V, L, B] =
    IOResult(io.map(_.mapWithStore(f)))

}

private[caio] object ResultOps {

  def fromIO[C, V, L:Monoid, A](ioa:IO[A]):IOResult[C, V, L, A] =
    IOResult(ioa.map(a => SuccessResult[C, V, L, A](a, Store.empty[C, L])).
      handleErrorWith(ex => IO(ErrorResult[C, V, L, A](Left(ex), Store.empty[C, L]))))

  @inline
  def tryCatchPure[C, V, L, A](store:Store[C, L])(f: => PureResult[C, V, L, A]):PureResult[C, V, L, A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Left(ex), store)
    }

  @inline
  def tryCatch[C, V, L, A](store:Store[C, L])(f: => Result[C, V, L, A]):Result[C, V, L, A] =
    try {
      f
    } catch {
      case NonFatal(ex) =>
        ErrorResult(Left(ex), store)
    }
}

