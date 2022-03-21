package caio

import caio.Event.EventLog
import cats.Monoid
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

import scala.util.Either

package object arbitrary {
  implicit def arbitraryForCaio[C: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Arbitrary[Caio[C, L, A]] =
    Arbitrary(Gen.delay(genCaio[C, L, A]))

  def genCaio[C: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    Gen.frequency(
      1 -> genPure[C, L, A],
      1 -> genApply[C, L, A],
      1 -> genError[C, L, A],
      1 -> genAsync[C, L, A],
      1 -> genNestedAsync[C, L, A],
      1 -> genTell[C, L, A],
      1 -> genContext[C, L, A],
      1 -> getMapOne[C, L, A],
      1 -> getMapTwo[C, L, A],
      2 -> genFlatMap[C, L, A]
    )

  def genPure[C, L, A: Arbitrary]: Gen[Caio[C, L, A]] =
    getArbitrary[A].map(Caio.pure)

  def genApply[C, L, A: Arbitrary]: Gen[Caio[C, L, A]] =
    getArbitrary[A].map(Caio.apply[A](_))

  def genError[C, L, A]: Gen[Caio[C, L, A]] =
    getArbitrary[Throwable].map(Caio.raiseError)

  def genAsync[C, L, A: Arbitrary]: Gen[Caio[C, L, A]] =
    getArbitrary[(Either[Throwable, A] => Unit) => Unit].map(Caio.async_[A])

  def genTell[C, L: Monoid: Arbitrary, A: Arbitrary]: Gen[Caio[C, L, A]] =
    getArbitrary[L].flatMap(l => genPure[C, L, A].map(Caio.tell(l) *> _))

  def genContext[C: Arbitrary, L, A: Arbitrary]: Gen[Caio[C, L, A]] =
    getArbitrary[C].flatMap(c => genPure[C, L, A].map(Caio.setContext(c) *> _))

  def genNestedAsync[C: Arbitrary, L: Monoid: Arbitrary, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    getArbitrary[(Either[Throwable, Caio[C, L, A]] => Unit) => Unit]
      .map(k => Caio.async_(k).flatMap(x => x))

  def genBindSuspend[C, L, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    getArbitrary[A].map(Caio.apply(_).flatMap(Caio.pure))

  def genFlatMap[C: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, L, A]]
      f    <- getArbitrary[A => Caio[C, L, A]]
    } yield caio.flatMap(f)

  def getMapOne[C: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, L, A]]
      f    <- getArbitrary[A => A]
    } yield caio.map(f)

  def getMapTwo[C: Arbitrary, L: Arbitrary: Monoid, A: Arbitrary: Cogen]: Gen[Caio[C, L, A]] =
    for {
      caio <- getArbitrary[Caio[C, L, A]]
      f1   <- getArbitrary[A => A]
      f2   <- getArbitrary[A => A]
    } yield caio.map(f1).map(f2)

  implicit def arbitraryForEventLog: Arbitrary[EventLog] =
    Arbitrary(getArbitrary[Int].map(TestEvent(_)).map(Vector(_)))

  implicit def cogenForEvent: Cogen[Event] =
    Cogen.cogenInt.contramap[Event](_.asInstanceOf[TestEvent].id)
}