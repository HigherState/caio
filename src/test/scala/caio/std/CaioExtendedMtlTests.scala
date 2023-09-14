package caio.std

import caio.*
import caio.mtl.{ContextProjector, Extender, InvariantAsk, Provider}
import cats.{Monad, MonadError, MonadThrow}
import cats.effect.{IO, LiftIO, Sync}
import cats.mtl.{Listen, Stateful}
import caio.mtl.Extends
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers

class CaioExtendedMtlTests extends AsyncFunSpec with Matchers {
  import cats.implicits._

  type InvariantAskAtomic1[F[_]] = InvariantAsk[F, Atomic1]
  type InvariantAskAtomic2[F[_]] = InvariantAsk[F, Atomic2]
  type StatefulAtomic3[F[_]] = Stateful[F, Atomic3]
  type ListenEvent[F[_]] = Listen[F, Event]
  type ExtenderAtomic1[F[_]] = Extender[F, Atomic1]

  class Nested1[M[_]: InvariantAskAtomic1: ListenEvent: Monad] {

    def eval(): M[String] =
      for {
        a <- implicitly[InvariantAsk[M, Atomic1]].ask
        _ <- implicitly[Listen[M, Event]].writer((), Event.event1)
      } yield "Nested1:" + a.value
  }

  class Nested2[M[_]: InvariantAskAtomic2: LiftIO: MonadThrow] {

    def eval(): M[String] =
      for {
        a    <- implicitly[InvariantAsk[M, Atomic2]].ask
        time <- implicitly[LiftIO[M]].liftIO(IO.delay(System.currentTimeMillis()))
      } yield "Nested2:" + a.value + ":" + time

    def evalError(): M[String] =
      eval().flatMap { _ =>
        implicitly[MonadError[M, Throwable]].raiseError(Exception.exception1)
      }

    def evalHandle(): M[String] =
      implicitly[MonadError[M, Throwable]]
        .handleError(evalError())(e => "Nested2:exception:" + e.toString)
  }

  class Nested3[M[_]: StatefulAtomic3: Sync] {
    def eval(): M[String] =
      for {
        ms <- implicitly[Stateful[M, Atomic3]].get
        _  <- implicitly[Stateful[M, Atomic3]].set(new Atomic3("Nested3:" + ms.value))
      } yield "Nested3:" + ms.value
  }

  class ExtendsNested[M[_]: ExtenderAtomic1: LiftIO: ListenEvent: Sync] {

    val nested1: Nested1[M] = {
      import ContextProjector._
      new Nested1[M]
    }

    val E2: Extends[M,Atomic1,Atomic2] = implicitly[Extender[M, Atomic1]].apply[Atomic2]
    val nested2: Nested2[E2.FE] = {
      import E2._
      new Nested2[E2.FE]
    }

    val E3: Extends[M,Atomic1,Atomic3] = implicitly[Extender[M, Atomic1]].apply[Atomic3]
    val nested3: Nested3[E3.FE] = {
      import E3._
      new Nested3[E3.FE]
    }

    def eval(atomic2: Atomic2, atomic3: Atomic3): M[(String, String, String)] =
      for {
        n1 <- nested1.eval()
        n2 <- E2.apply(atomic2)(nested2.eval())
        n3 <- E3.apply(atomic3)(nested3.eval())
      } yield (n1, n2, n3)

    def evalError(atomic2: Atomic2, atomic3: Atomic3): M[(String, String)]       =
      for {
        n3 <- E3.apply(atomic3)(nested3.eval())
        n2 <- E2.apply(atomic2)(nested2.evalError())
      } yield n2 -> n3

    def evalErrorHandle(atomic2: Atomic2, atomic3: Atomic3): M[(String, String)] =
      for {
        n3 <- E3.apply(atomic3)(nested3.eval())
        n2 <- E2.apply(atomic2)(nested2.evalHandle())
      } yield n2 -> n3
  }

  class ExtendsProvided[M[_]: Provider: LiftIO: ListenEvent: MonadThrow: Sync] {

    val E: Extends[M,Unit,Atomic1]             = implicitly[Provider[M]].apply[Atomic1]
    import E._
    val extendsNested = new ExtendsNested[E.FE]

    def eval(atomic1: Atomic1, atomic2: Atomic2, atomic3: Atomic3): M[(String, String, String)] =
      E.apply(atomic1)(extendsNested.eval(atomic2, atomic3))

    def evalError(atomic1: Atomic1, atomic2: Atomic2, atomic3: Atomic3): M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalError(atomic2, atomic3))

    def evalErrorHandle(atomic1: Atomic1, atomic2: Atomic2, atomic3: Atomic3): M[(String, String)] =
      E.apply(atomic1)(extendsNested.evalErrorHandle(atomic2, atomic3))
  }
}
