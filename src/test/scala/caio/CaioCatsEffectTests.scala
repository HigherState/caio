package caio

import java.util.concurrent.atomic.AtomicInteger

import caio.std.{CaioParApplicative, CaioParallel}
import cats.data.NonEmptyList
import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, IO}
import cats.effect.laws.discipline.ConcurrentEffectTests
import cats.kernel.laws.IsEqArrow
import cats.laws.discipline.{CommutativeApplicativeTests, CommutativeMonadTests, MonadErrorTests, ParallelTests}
import org.scalacheck.Prop.forAll

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class CaioCatsEffectTests extends TestInstances {
  import arbitrary._
  import cats.effect.laws.discipline.arbitrary._

  checkAllAsync("Caio") { params =>
    import params._
    CommutativeMonadTests[CaioT].monad[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    MonadErrorTests[CaioT, Throwable].monadError[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    ConcurrentEffectTests[CaioT](CE, EC.contextShift[CaioT](CE)).concurrentEffect[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    val CP = new CaioParallel[C, V, L]
    val module = ParallelTests[CaioT](CP)
    module.parallel[Int, Int]
  }

  checkAllAsync("ParCaio") { params =>
    import params._
    implicit val CA = new CaioParApplicative[C, V, L](CE)
    CommutativeApplicativeTests[ParCaio[C, V, L, ?]].commutativeApplicative[Int, Int, Int]
  }

  testGlobalAsync("defer evaluation until run") { params =>
    var run = false
    val caio = Caio { run = true }
    assertEquals(run, false)
    params.CE.toIO(caio).unsafeRunSync()
    assertEquals(run, true)
  }

  testAsync("throw in register is fail") { params =>
    import params.EC

    forAll { (e: Throwable) =>
      (Caio.async[Unit](_ => throw e): CaioT[Unit]) <-> Caio.raiseError(e)
    }
  }

  testAsync("thrown exceptions after callback was called once are re-thrown") { params =>
    val dummy = new RuntimeException("dummy")
    val caio = Caio.async[Int] { cb =>
      cb(Right(10))
      throw dummy
    }

    var effect: Option[Either[Throwable, Int]] = None

    val sysErr = catchSystemErr {
      params.CE.toIO(caio).unsafeRunAsync { v =>
        effect = Some(v)
      }
      params.EC.tick()
    }

    assertEquals(effect, Some(Right(10)))
    assert(sysErr.contains("dummy"))
  }

  testGlobalAsync("catch exceptions within main block") { params =>
    case object Foo extends Exception

    val caio = Caio(throw Foo)

    assertEquals(params.CE.toIO(caio.attempt).unsafeRunSync().left.toOption.get, Foo)
  }

  testGlobalAsync("fromEither handles Throwable in Left Projection") { params =>
    case object Foo extends Exception
    val e: Either[Throwable, Nothing] = Left(Foo)

    assertEquals(params.CE.toIO(Caio.fromEither(e).attempt).unsafeRunSync().left.toOption.get, Foo)
  }

  testGlobalAsync("fromEither handles a Value in Right Projection") { params =>
    case class Foo(x: Int)
    val e: Either[Throwable, Foo] = Right(Foo(1))

    assertEquals(params.CE.toIO(Caio.fromEither(e).attempt).unsafeRunSync().toOption.get, Foo(1))
  }

  testGlobalAsync("fromEitherFailure handles V in Left Projection") { params =>
    case object Foo
    val e: Either[Foo.type, Nothing] = Left(Foo)

    assertEquals(params.CE.toIO(Caio.fromEitherFailure(e).either).unsafeRunSync().left.toOption.get, NonEmptyList.of(Foo))
  }

  testGlobalAsync("fromEitherFailure handles a Value in Right Projection") { params =>
    case class Foo(x: Int)
    val e: Either[Unit, Foo] = Right(Foo(1))

    assertEquals(params.CE.toIO(Caio.fromEitherFailure(e).either).unsafeRunSync().toOption.get, Foo(1))
  }

  testGlobalAsync("fromTry handles Failure") { params =>
    case object Foo extends Exception
    val t: Try[Nothing] = Failure(Foo)

    assertEquals(params.CE.toIO(Caio.fromTry(t).attempt).unsafeRunSync().left.toOption.get, Foo)
  }

  testGlobalAsync("fromTry handles Success") { params =>
    case class Foo(x: Int)
    val t: Try[Foo] = Success(Foo(1))

    assertEquals(params.CE.toIO(Caio.fromTry(t).attempt).unsafeRunSync().toOption.get, Foo(1))
  }

  testAsync("Caio.async protects against multiple callback calls") { params =>
    val effect = new AtomicInteger()

    val caio = Caio.async[Int] { cb =>
      cb(Right(10))
      cb(Right(20))
    }

    params.CE.toIO(caio).unsafeRunAsync {
      case Right(v) => effect.addAndGet(v); ()
      case Left(ex) => throw ex
    }

    params.EC.tick()

    assertEquals(effect.get, 10)
  }

  testAsync("Caio.async protects against thrown exceptions") { params =>
    val dummy = new RuntimeException("dummy")
    val caio = Caio.async[Int] { _ => throw dummy }
    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Failure(dummy)))
  }

  testAsync("Caio.async does not break referential transparency") { params =>
    val caio = Caio.async[Int](_(Right(10)))
    val sum = for (a <- caio; b <- caio; c <- caio) yield a + b + c
    val f = params.CE.toIO(sum).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(30)))
  }

  testAsync("fromFuture works for values") { params =>
    import params._

    forAll { (a: Int, f: Int => Long) =>
      Caio.fromFuture[C, V, L, Long](Caio(Future(f(a)))) <-> Caio(f(a))
    }
  }

  testAsync("fromFuture works for successful completed futures") { params =>
    import params._

    forAll { (a: Int) =>
      Caio.fromFuture[C, V, L, Int](Caio.pure(Future.successful(a))) <-> Caio.pure(a)
    }
  }

  testAsync("fromFuture works for exceptions") { params =>
    import params._

    forAll { (ex: Throwable) =>
      val caio = Caio.fromFuture[C, V, L, Int](Caio(Future(throw ex)))
      caio <-> Caio.raiseError(ex)
    }
  }

  testAsync("fromFuture works for failed completed futures") { params =>
    import params._

    forAll { (ex: Throwable) =>
      Caio.fromFuture[C, V, L, Int](Caio.pure(Future.failed(ex))) <-> Caio.raiseError(ex)
    }
  }

  testAsync("fromFuture protects against user code") { params =>
    import params._

    forAll { (ex: Throwable) =>
      val caio = Caio.fromFuture[C, V, L, Int](Caio(throw ex))
      caio <-> Caio.raiseError(ex)
    }
  }

  testAsync("fromFuture suspends side-effects") { params =>
    import params._

    forAll { (a: Int, f: (Int, Int) => Int, g: (Int, Int) => Int) =>
      var effect = a
      val caio1 = Caio.fromFuture[C, V, L, Int](Caio(Future { effect = f(effect, a); effect }))
      val caio2 = Caio.fromFuture[C, V, L, Int](Caio(Future { effect = g(effect, a); effect }))

      caio2.flatMap(_ => caio1).flatMap(_ => caio2) <-> Caio(g(f(g(a, a), a), a))
    }
  }

  testAsync("attempt flatMap loop") { params =>
    def loop[A](source: CaioT[A], n: Int): CaioT[A] =
      source.attempt.flatMap {
        case Right(l) =>
          if (n <= 0) Caio.pure(l)
          else loop(source, n - 1)
        case Left(e) =>
          Caio.raiseError(e)
      }

    val f = params.CE.toIO(loop(Caio("value"), 10000)).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success("value")))
  }

  testAsync("attempt foldLeft sequence") { params =>
    val count = 10000
    val loop = (0 until count).foldLeft(Caio(0)) { (acc, _) =>
      acc.attempt.flatMap {
        case Right(x) => Caio.pure(x + 1)
        case Left(e)  => Caio.raiseError(e)
      }
    }

    val f = params.CE.toIO(loop).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(count)))
  }

  testAsync("Caio(throw ex).attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio = Caio[Int](throw dummy).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("Caio(throw ex).flatMap.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio = Caio[Int](throw dummy).flatMap(Caio.pure).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("Caio(throw ex).map.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio = Caio[Int](throw dummy).map(x => x).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val source = Caio.async[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.flatMap.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val source = Caio.async[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.flatMap(Caio.pure).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.attempt.flatMap") { params =>
    val dummy = new RuntimeException("dummy")
    val source = Caio.async[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.attempt.flatMap {
      case Left(`dummy`) => Caio.pure(100)
      case _             => Caio.pure(0)
    }

    val f = params.CE.toIO(caio).unsafeToFuture()

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testGlobalAsync("bracket signals the error in use") { params =>
    val e = new RuntimeException("error in use")

    val caio = Caio.unit
      .bracket[C, V, L, Unit, Unit](_ => Caio.raiseError(e))(_ => Caio.unit)
      .attempt

    val r = params.CE.toIO(caio).unsafeRunSync()

    assertEquals(r, Left(e))
    assert(e.getSuppressed.isEmpty)
  }

  testGlobalAsync("bracket signals the error in release") { params =>
    val e = new RuntimeException("error in release")

    val caio = Caio.unit
      .bracket[C, V, L, Unit, Unit](_ => Caio.unit)(_ => Caio.raiseError(e))
      .attempt

    val r = params.CE.toIO(caio).unsafeRunSync()

    assertEquals(r, Left(e))
    assert(e.getSuppressed.isEmpty)
  }

  testGlobalAsync("bracket signals the error in use and logs the error from release") { params =>
    val e1 = new RuntimeException("error in use")
    val e2 = new RuntimeException("error in release")

    var r: Option[Either[Throwable, Nothing]] = None
    val sysErr = catchSystemErr {
      val caio =
        Caio.unit
          .bracket[C, V, L, Unit, Nothing](_ => Caio.raiseError(e1))(_ => Caio.raiseError(e2))
          .attempt

      r = Some(params.CE.toIO(caio).unsafeRunSync())
    }

    assertEquals(r, Some(Left(e1)))
    assert(sysErr.contains("error in release"))
    assert(e1.getSuppressed.isEmpty)
    assert(e2.getSuppressed.isEmpty)
  }

  testAsync("bracket does not evaluate use on cancel") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    var use = false
    var release = false

    val task = Caio
      .sleep(2.second)
      .bracket[C, V, L, Unit, Unit](_ => Caio { use = true })(_ => Caio { release = true })
      .timeoutTo[C, V, L, Unit](1.second, Caio.never)

    val f = params.CE.toIO(task).unsafeToFuture()

    params.EC.tick(2.second)

    assertEquals(f.value, None)
    assertEquals(use, false)
    assertEquals(release, true)
  }

  testAsync("start forks automatically") { params =>
    import params._

    val f = CE.toIO(Caio(1).start[C, V, L, Int].flatMap(_.join)).unsafeToFuture()
    assertEquals(f.value, None)
    EC.tick()
    assertEquals(f.value, Some(Success(1)))
  }

  testAsync("background cancels the action in cleanup") { params =>
    import params._

    val caio = Deferred[CaioT, Unit]
      .flatMap { started =>
        Deferred[CaioT, ExitCase[Throwable]].flatMap { result =>
          val bg = started.complete(()) *> Caio.never.guaranteeCase(result.complete)

          bg.background.use(_ => started.get) *> result.get
        }
      }

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()

    assertEquals(f.value, Some(Success(ExitCase.Canceled)))
  }

  testAsync("background allows awaiting the action") { params =>
    import params._

    val caio = Deferred[CaioT, Unit]
      .flatMap { latch =>
        val bg = latch.get *> Caio.pure(42)

        bg.background.use(await => latch.complete(()) *> await)
      }

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()

    assertEquals(f.value, Some(Success(42)))
  }

  testAsync("cancel should wait for already started finalizers on success") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val caio = for {
      pa <- Deferred[CaioT, Unit]
      fiber <- Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(1.second)).start[C, V, L, Unit]
      _ <- pa.get
      _ <- fiber.cancel
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, None)

    EC.tick(1.second)
    assertEquals(f.value, Some(Success(())))
  }

  testAsync("cancel should wait for already started finalizers on failure") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val dummy = new RuntimeException("dummy")

    val caio = for {
      pa <- Deferred[CaioT, Unit]
      fiber <- Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(1.second) *> Caio.raiseError(dummy)).start[C, V, L, Unit]
      _ <- pa.get
      _ <- fiber.cancel
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, None)

    EC.tick(1.second)
    assertEquals(f.value, Some(Success(())))
  }

  testAsync("cancel should wait for already started use finalizers") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val caio = for {
      pa <- Deferred[CaioT, Unit]
      fibA <- Caio.unit
        .bracket[C, V, L, Unit, Unit](_ => Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(2.second)))(_ => Caio.unit)
        .start[C, V, L, Unit]
      _ <- pa.get
      _ <- fibA.cancel
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, None)

    EC.tick(2.second)
    assertEquals(f.value, Some(Success(())))
  }

  testAsync("second cancel should wait for use finalizers") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val caio = for {
      pa <- Deferred[CaioT, Unit]
      fiber <- Caio.unit
        .bracket[C, V, L, Unit, Unit](_ => (pa.complete(()) *> Caio.never).guarantee(Caio.sleep(2.second)))(_ => Caio.unit)
        .start[C, V, L, Unit]
      _ <- pa.get
      _ <- Caio.race(fiber.cancel, fiber.cancel)
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, None)

    EC.tick(2.second)
    assertEquals(f.value, Some(Success(())))
  }

  testAsync("second cancel during acquire should wait for it and finalizers to complete") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val caio = for {
      pa <- Deferred[CaioT, Unit]
      fiber <- (pa.complete(()) *> Caio.sleep(1.second))
        .bracket[C, V, L, Unit, Unit](_ => Caio.unit)(_ => Caio.sleep(1.second))
        .start[C, V, L, Unit]
      _ <- pa.get
      _ <- Caio.race(fiber.cancel, fiber.cancel)
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, None)

    EC.tick(1.second)
    assertEquals(f.value, None)

    EC.tick(1.second)
    assertEquals(f.value, Some(Success(())))
  }

  testAsync("second cancel during acquire should wait for it and finalizers to complete (non-terminating)") {
    params =>
      import params._

      implicit val timer = getTimer(params.EC.timer[IO])

      val caio = for {
        pa <- Deferred[CaioT, Unit]
        fiber <- (pa.complete(()) *> Caio.sleep(1.second))
          .bracket[C, V, L, Unit, Unit](_ => Caio.unit)(_ => Caio.never)
          .start[C, V, L, Unit]
        _ <- pa.get
        _ <- Caio.race(fiber.cancel, fiber.cancel)
      } yield ()

      val f = CE.toIO(caio).unsafeToFuture()

      EC.tick()
      assertEquals(f.value, None)

      EC.tick(1.day)
      assertEquals(f.value, None)
  }

  testAsync("Multiple cancel should not hang") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    val caio = for {
      fiber <- Caio.sleep(1.second).start[C, V, L, Unit]
      _ <- fiber.cancel
      _ <- fiber.cancel
    } yield ()

    val f = CE.toIO(caio).unsafeToFuture()

    EC.tick()
    assertEquals(f.value, Some(Success(())))
  }
}
