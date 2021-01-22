package caio.std

import caio.Event._
import caio.implicits.StaticImplicits
import caio.mtl.ApplicativeFail
import caio.{Caio, Failure}

import cats.{ Applicative, ApplicativeError, Monoid }
import cats.syntax.parallel._
import cats.syntax.apply._
import cats.data.NonEmptyList
import cats.mtl.{ FunctorListen, FunctorTell, MonadState }
import cats.mtl.syntax.listen._
import cats.effect.{Clock, ContextShift, IO, Timer}
import cats.effect.concurrent.Ref
import org.scalatest.{AsyncFunSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class CaioConcurrentEffectTests extends AsyncFunSpec with Matchers{

  type CaioT[A] = Caio[Int, Failure, EventLog, A]

  implicit val CS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val C = new StaticImplicits[Int, Failure, EventLog] {
    protected implicit def ML: Monoid[EventLog] = EventMonoid
  }

  import C._

  val effect = new CaioConcurrentEffect[Int, Failure, EventLog](0)((_, _) => IO.unit)((_, _, _) => IO.unit)((_, _, _) => IO.unit)

  val T: Timer[IO] = IO.timer(ExecutionContext.global)

  val timer = new Timer[CaioT] {
    val clock: Clock[CaioT]                          = Clock.create[CaioT]
    def sleep(duration: FiniteDuration): CaioT[Unit] = staticCaioConcurrent.liftIO(T.sleep(duration))
  }

  def run[A](caio: CaioT[A]): A =
    effect.toIO(caio).unsafeRunSync()

  describe("Async shouldnt loop") {
    it("Works with timer async case") {
      run(timer.sleep(1.millis)) shouldBe ()
    }
  }

  describe("Concurrency") {
    it("Should handle computations in parallel") {
      val random  = new scala.util.Random
      val numbers = NonEmptyList.fromListUnsafe(Range(0, 100).toList)
      val program =
        for {
          ref  <- Ref.of[CaioT, List[Int]](Nil)
          _    <- numbers.parTraverse_ { n => timer.sleep(random.nextInt(100).millis) *> ref.update(_ :+ n) }
          list <- ref.get
        } yield NonEmptyList.fromListUnsafe(list)

      val result = run(program)

      result should not be equal (numbers)
      result.toList should contain theSameElementsAs (numbers.toList)
    }

    it("Should handle failures properly") {
      def makeIOList(failFirst: Boolean, failSecond: Boolean): NonEmptyList[CaioT[Int]] =
        NonEmptyList.of(
          if (failFirst) ApplicativeFail[CaioT, Failure].fail[Int](new Failure("1")) else Applicative[CaioT].pure(1),
          if (failSecond) ApplicativeFail[CaioT, Failure].fail[Int](new Failure("2")) else Applicative[CaioT].pure(2)
        )

      def program(failFirst: Boolean, failSecond: Boolean): CaioT[NonEmptyList[Int]] =
        ApplicativeFail[CaioT, Failure].resolve(makeIOList(failFirst, failSecond).parSequence) { case failures =>
          failures.map(failure => -failure.value.toInt)
        }

      run(program(true, false)) should be equals (NonEmptyList.of(-1))
      run(program(false, true)) should be equals (NonEmptyList.of(-2))
      run(program(false, false)).toList should contain theSameElementsAs List(1, 2)
      run(program(true, true)).toList should contain oneOf(-1, -2)
    }

     it("Should handle errors properly") {
      def makeIOList(failFirst: Boolean, failSecond: Boolean): NonEmptyList[CaioT[Int]] =
        NonEmptyList.of(
          if (failFirst) ApplicativeError[CaioT, Throwable].raiseError[Int](new Exception("1")) else Applicative[CaioT].pure(1),
          if (failSecond) ApplicativeError[CaioT, Throwable].raiseError[Int](new Exception("2")) else Applicative[CaioT].pure(2)
        )

      def program(failFirst: Boolean, failSecond: Boolean): CaioT[NonEmptyList[Int]] =
        ApplicativeError[CaioT, Throwable].handleError(makeIOList(failFirst, failSecond).parSequence) { throwable =>
          NonEmptyList.of(-throwable.getMessage.toInt)
        }

      run(program(true, false)) should be equals (NonEmptyList.of(-1))
      run(program(false, true)) should be equals (NonEmptyList.of(-2))
      run(program(false, false)).toList should contain theSameElementsAs List(1, 2)
      run(program(true, true)).toList should contain oneOf(-1, -2)
    }

    it("Should handle logs when a fiber is joined") {
      val program =
        FunctorListen[CaioT, EventLog].listen {
          for {
            fiber <- effect.start(FunctorTell[CaioT, EventLog].tell(Vector(event1)))
            _     <- FunctorTell[CaioT, EventLog].tell(Vector(event2))
            _     <- fiber.join
          } yield ()
        }

      run(program)._2 should contain theSameElementsAs Vector(event1, event2)
    }

    it("Should handle logs properly") {
      def makeIOList(logFirst: Boolean, logSecond: Boolean): NonEmptyList[CaioT[Unit]] =
        NonEmptyList.of(
          if (logFirst) FunctorTell[CaioT, EventLog].tell(Vector(event1)) else Applicative[CaioT].unit,
          if (logSecond) FunctorTell[CaioT, EventLog].tell(Vector(event2)) else Applicative[CaioT].unit
        )

      def program(logFirst: Boolean, logSecond: Boolean): CaioT[EventLog] =
        makeIOList(logFirst, logSecond).parSequence.listen.map(_._2)

      run(program(true, false)) should be equals (NonEmptyList.of(event1))
      run(program(false, true)) should be equals (NonEmptyList.of(event2))
      run(program(true, true)).toList should contain theSameElementsAs List(event1, event2)
    }

    it("Should set state when a fiber is joined") {
      val program =
        for {
          fiber <- effect.start(MonadState[CaioT, Int].set(1))
          _     <- fiber.join
          state <- MonadState[CaioT, Int].get
        } yield state

      run(program) shouldBe 1
    }

    it("Should set state in parallel") {
      def makeIOList(setFirst: Boolean, setSecond: Boolean): NonEmptyList[CaioT[Unit]] =
        NonEmptyList.of(
          if (setFirst) MonadState[CaioT, Int].set(1) else Applicative[CaioT].unit,
          if (setSecond) MonadState[CaioT, Int].set(2) else Applicative[CaioT].unit
        )

      def program(setFirst: Boolean, setSecond: Boolean): CaioT[Int] =
        makeIOList(setFirst, setSecond).parSequence *> MonadState[CaioT, Int].get

      List(0, 1) should contain (run(program(true, false)))
      List(0, 2) should contain (run(program(false, true)))
      List(1, 2) should contain (run(program(true, true)))
      run(program(false, false)) shouldBe 0
    }
  }
}
