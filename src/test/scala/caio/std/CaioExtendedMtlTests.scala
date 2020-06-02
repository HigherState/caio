package caio.std

import caio._
import caio.mtl.{ApplicativeFail, ContextProjector, Extender, Provider}
import cats.{Applicative, Monad, MonadError}
import cats.effect.{IO, LiftIO, Sync}
import cats.mtl.{ApplicativeAsk, FunctorListen, MonadState}
import org.scalatest.{AsyncFunSpec, Matchers}

class CaioExtendedMtlTests extends AsyncFunSpec with Matchers{
  import cats.implicits._

  class Nested1[
    M[_]
      :ApplicativeAsk[*[_], Atomic1]
      :FunctorListen[*[_], Event]
      :ApplicativeFail[*[_], Failure]:Monad]{

    def eval():M[String] =
    for {
      a <- implicitly[ApplicativeAsk[M, Atomic1]].ask
      _ <- implicitly[FunctorListen[M, Event]].writer((), Event.event1)
    } yield "Nested1:" + a.value

    def evalFail():M[String] =
      eval().flatMap{a =>
        implicitly[ApplicativeFail[M, Failure]].fail(Failure.failure1)
      }

    def evalHandle():M[String] =
      implicitly[ApplicativeFail[M, Failure]].handleFailuresWith(evalFail()){nl =>
        implicitly[Applicative[M]].pure("Nested1:failure:" + nl.head.value)
      }
  }

  class Nested2[
    M[_]
      :ApplicativeAsk[*[_], Atomic2]
      :LiftIO:MonadError[*[_], Throwable]]{

    def eval():M[String] =
      for {
        a <- implicitly[ApplicativeAsk[M, Atomic2]].ask
        time <- implicitly[LiftIO[M]].liftIO(IO.delay(System.currentTimeMillis()))
      } yield "Nested2:" + a.value + ":" + time

    def evalError():M[String] =
      eval().flatMap{_ =>
        implicitly[MonadError[M, Throwable]].raiseError(Exception.exception1)
      }

    def evalHandle():M[String] =
      implicitly[MonadError[M, Throwable]]
        .handleError(evalError())(e => "Nested2:exception:" + e.toString)
  }

  class Nested3[M[_]:MonadState[*[_], Atomic3]:Sync]{

    def eval():M[String] =
      for {
        ms <- implicitly[MonadState[M, Atomic3]].get
        _ <- implicitly[MonadState[M, Atomic3]].set(new Atomic3("Nested3:" + ms.value))
      } yield "Nested3:" + ms.value
  }

  class ExtendsNested[
    M[_]:Extender[*[_], Atomic1]
      :LiftIO
      :FunctorListen[*[_], Event]
      :ApplicativeFail[*[_], Failure]
      :Sync] {


    val nested1 = {
      import ContextProjector._
      new Nested1[M]
    }

    val E2 = implicitly[Extender[M, Atomic1]].apply[Atomic2]
    val nested2: Nested2[E2.FE] =  {
      import E2._
      new Nested2[E2.FE]
    }

    val E3 = implicitly[Extender[M, Atomic1]].apply[Atomic3]
    val nested3 = {
      import E3._
      new Nested3[E3.FE]
    }

    def eval(atomic2:Atomic2, atomic3:Atomic3):M[(String, String, String)] =
      for {
        n1 <- nested1.eval()
        n2 <- E2.apply(atomic2)(nested2.eval())
        n3 <- E3.apply(atomic3)(nested3.eval())
      } yield (n1, n2, n3)

    def evalFail(atomic2:Atomic2):M[(String, String)] =
      for {
        n2 <- E2.apply(atomic2)(nested2.eval())
        n1 <- nested1.evalFail()
      } yield n1 -> n2

    def evalError(atomic2:Atomic2, atomic3:Atomic3):M[(String, String)] =
      for {
        n3 <- E3.apply(atomic3)(nested3.eval())
        n2 <- E2.apply(atomic2)(nested2.evalError())
      } yield n2 -> n3

    def evalFailHandle(atomic2:Atomic2):M[(String, String)] =
      for {
        n2 <- E2.apply(atomic2)(nested2.eval())
        n1 <- nested1.evalHandle()
      } yield n1 -> n2

    def evalErrorHandle(atomic2:Atomic2, atomic3:Atomic3):M[(String, String)] =
      for {
        n3 <- E3.apply(atomic3)(nested3.eval())
        n2 <- E2.apply(atomic2)(nested2.evalHandle())
      } yield n2 -> n3
  }

  class ExtendsProvided[
    M[_]:Provider:LiftIO:FunctorListen[*[_], Event]:MonadError[*[_], Throwable]:ApplicativeFail[*[_], Failure]:Sync] {

    val E = implicitly[Provider[M]].apply[Atomic1]
    import E._
    val extendsNested = new ExtendsNested[E.FE]

    def eval(atomic1:Atomic1, atomic2:Atomic2, atomic3:Atomic3):M[(String, String, String)] =
      E.apply(atomic1)(extendsNested.eval(atomic2, atomic3))

    def evalFail(atomic1:Atomic1, atomic2:Atomic2):M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalFail(atomic2))

    def evalError(atomic1:Atomic1, atomic2:Atomic2, atomic3:Atomic3):M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalError(atomic2, atomic3))

    def evalFailHandle(atomic1:Atomic1, atomic2:Atomic2):M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalFailHandle(atomic2))

    def evalErrorHandle(atomic1:Atomic1, atomic2:Atomic2, atomic3:Atomic3):M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalErrorHandle(atomic2, atomic3))

  }
}
