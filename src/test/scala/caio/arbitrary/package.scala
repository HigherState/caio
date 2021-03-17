package caio

import caio.Event.EventLog
import caio.std.Par
import cats.Monoid
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

import scala.util.Either

package object arbitrary {
  implicit def arbitraryForCaio[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Arbitrary[Caio[C, V, L, A]] =
    Arbitrary(Gen.delay(genCaio[C, V, L, A]))

  implicit def arbitraryForParCaio[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Arbitrary[ParCaio[C, V, L, A]] =
    Arbitrary(arbitraryForCaio[C, V, L, A].arbitrary.map(Par.apply))

  def genCaio[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    Gen.frequency(
      1 -> genPure[C, V, L, A],
      1 -> genApply[C, V, L, A],
      1 -> genError[C, V, L, A],
      1 -> genFail[C, V, L, A],
      1 -> genAsync[C, V, L, A],
      1 -> genAsyncF[C, V, L, A],
      1 -> genNestedAsync[C, V, L, A],
      1 -> genTell[C, V, L, A],
      1 -> genContext[C, V, L, A],
      1 -> getMapOne[C, V, L, A],
      1 -> getMapTwo[C, V, L, A],
      2 -> genFlatMap[C, V, L, A]
    )

  def genPure[C, V, L, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[A].map(Caio.pure)

  def genApply[C, V, L, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[A].map(Caio.apply[A](_))

  def genError[C, V, L, A]: Gen[Caio[C, V, L, A]] =
    getArbitrary[Throwable].map(Caio.raiseError)

  def genFail[C, V: Arbitrary, L, A]: Gen[Caio[C, V, L, A]] =
    getArbitrary[V].map(Caio.fail(_))

  def genAsync[C, V, L, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[(Either[Throwable, A] => Unit) => Unit].map(Caio.async)

  def genAsyncF[C, V, L: Monoid, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[(Either[Throwable, A] => Unit) => Unit].map { k =>
      Caio.asyncF[C, V, L, A](cb => Caio(k(cb)))
    }

  def genTell[C, V, L: Arbitrary, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[L].flatMap(l => genPure[C, V, L, A].map(Caio.tell(l) *> _))

  def genContext[C: Arbitrary, V, L, A: Arbitrary]: Gen[Caio[C, V, L, A]] =
    getArbitrary[C].flatMap(c => genPure[C, V, L, A].map(Caio.setContext(c) *> _))

  def genNestedAsync[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    getArbitrary[(Either[Throwable, Caio[C, V, L, A]] => Unit) => Unit]
      .map(k => Caio.async(k).flatMap(x => x))

  def genBindSuspend[C, V, L, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    getArbitrary[A].map(Caio.apply(_).flatMap(Caio.pure))

  def genFlatMap[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, V, L, A]]
      f    <- getArbitrary[A => Caio[C, V, L, A]]
    } yield caio.flatMap(f)

  def getMapOne[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, V, L, A]]
      f    <- getArbitrary[A => A]
    } yield caio.map(f)

  def getMapTwo[C: Arbitrary, V: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, V, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, V, L, A]]
      f1   <- getArbitrary[A => A]
      f2   <- getArbitrary[A => A]
    } yield caio.map(f1).map(f2)

  implicit def arbitraryForFailure: Arbitrary[Failure] =
    Arbitrary(getArbitrary[String].map(Failure(_)))

  implicit def arbitraryForEventLog: Arbitrary[EventLog] =
    Arbitrary(getArbitrary[Int].map(TestEvent(_)).map(Vector(_)))

  implicit def cogenForEvent: Cogen[Event] =
    Cogen.cogenInt.contramap[Event](_.asInstanceOf[TestEvent].id)
}