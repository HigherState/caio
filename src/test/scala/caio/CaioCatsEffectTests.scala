package caio

import java.util.concurrent.atomic.AtomicInteger

import cats.kernel.laws.IsEqArrow
import cats.effect.{Deferred, Outcome}
import cats.effect.laws.AsyncTests
import cats.laws.discipline.{CommutativeMonadTests, MonadErrorTests}
import org.scalacheck.Prop.forAll

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class CaioCatsEffectTests extends TestInstances {
  import arbitrary._

  checkAllAsync("Caio") { params =>
    import params._
    import implicits.dynamicCaioMonad
    CommutativeMonadTests[CaioT].monad[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    MonadErrorTests[CaioT, Throwable].monadError[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    AsyncTests[CaioT].async[Int, Int, Int](100.millis)
  }

  testAsync("defer evaluation until run") { params =>
    var run  = false
    val caio = Caio { run = true }
    assertEquals(run, false)
    params.CE.unsafeRunSync(caio)
    assertEquals(run, true)
  }

  testOneAsync("throw in register is fail") { params =>
    import params._

    forAll { (e: Throwable) =>
      (Caio.async[C, L, Unit](_ => throw e): CaioT[Unit]) <-> Caio.raiseError(e)
    }
  }

  testAsync("produce a failure when the registration raises an error after callback") { params =>
    val dummy = new RuntimeException("dummy")
    val caio  = Caio.async[C, L, Int](cb => Caio(cb(Right(10))).as(None)).flatMap(_ => Caio.raiseError(dummy)).attempt
    assertEquals(params.CE.unsafeRunSync(caio), Left(dummy))
  }

  testAsync("catch exceptions within main block") { params =>
    case object Foo extends Exception

    val caio = Caio(throw Foo)

    assertEquals(params.CE.unsafeRunSync(caio.attempt).left.toOption.get, Foo)
  }

  testAsync("fromEither handles Throwable in Left Projection") { params =>
    case object Foo extends Exception
    val e: Either[Throwable, Nothing] = Left(Foo)

    assertEquals(params.CE.unsafeRunSync(Caio.fromEither(e).attempt).left.toOption.get, Foo)
  }

  testAsync("fromEither handles a Value in Right Projection") { params =>
    case class Foo(x: Int)
    val e: Either[Throwable, Foo] = Right(Foo(1))

    assertEquals(params.CE.unsafeRunSync(Caio.fromEither(e).attempt).toOption.get, Foo(1))
  }

  testAsync("fromTry handles Failure") { params =>
    case object Foo extends Exception
    val t: Try[Nothing] = Failure(Foo)

    assertEquals(params.CE.unsafeRunSync(Caio.fromTry(t).attempt).left.toOption.get, Foo)
  }

  testAsync("fromTry handles Success") { params =>
    case class Foo(x: Int)
    val t: Try[Foo] = Success(Foo(1))

    assertEquals(params.CE.unsafeRunSync(Caio.fromTry(t).attempt).toOption.get, Foo(1))
  }

  testAsync("Caio.async protects against multiple callback calls") { params =>
    val effect = new AtomicInteger()

    val caio = Caio.async_[Int] { cb =>
      cb(Right(10))
      cb(Right(20))
    }

    params.CE.unsafeRunSync(caio.attempt) match {
      case Right(v) => effect.addAndGet(v); ()
      case Left(ex) => throw ex
    }

    assertEquals(effect.get, 10)
  }

  testAsync("Caio.async protects against thrown exceptions") { params =>
    val dummy = new RuntimeException("dummy")
    val caio  = Caio.async[Any, Nothing, Int](_ => throw dummy)
    val f     = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(scala.util.Failure(dummy)))
  }

  testAsync("Caio.async does not break referential transparency") { params =>
    val caio  = Caio.async_[Int](_(Right(10)))
    val sum   = for {
      a <- caio
      b <- caio
      c <- caio
    } yield a + b + c
    val value = params.CE.unsafeRunSync(sum)
    assertEquals(value, 30)
  }

  testOneAsync("fromFuture works for values") { params =>
    import params._

    forAll { (a: Int, f: Int => Long) =>
      Caio.fromFuture[C, L, Long](Caio(Future(f(a)))) <-> Caio(f(a))
    }
  }

  testOneAsync("fromFuture works for successful completed futures") { params =>
    import params._

    forAll { (a: Int) =>
      Caio.fromFuture[C, L, Int](Caio.pure(Future.successful(a))) <-> Caio.pure(a)
    }
  }

  testOneAsync("fromFuture works for exceptions") { params =>
    import params._

    forAll { (ex: Exception) =>
      val caio = Caio.fromFuture[C, L, Int](Caio(Future(throw ex)))
      caio <-> Caio.raiseError(ex)
    }
  }

  testOneAsync("fromFuture works for failed completed futures") { params =>
    import params._

    forAll { (ex: Exception) =>
      Caio.fromFuture[C, L, Int](Caio.pure(Future.failed(ex))) <-> Caio.raiseError(ex)
    }
  }

  testOneAsync("fromFuture protects against user code") { params =>
    import params._

    forAll { (ex: Throwable) =>
      val caio = Caio.fromFuture[C, L, Int](Caio(throw ex))
      caio <-> Caio.raiseError(ex)
    }
  }

  testOneAsync("fromFuture suspends side-effects") { params =>
    import params._

    forAll { (a: Int, f: (Int, Int) => Int, g: (Int, Int) => Int) =>
      var effect = a
      val caio1  = Caio.fromFuture[C, L, Int](Caio(Future { effect = f(effect, a); effect }))
      val caio2  = Caio.fromFuture[C, L, Int](Caio(Future { effect = g(effect, a); effect }))

      caio2.flatMap(_ => caio1).flatMap(_ => caio2) <-> Caio(g(f(g(a, a), a), a))
    }
  }

  testAsync("attempt flatMap loop") { params =>
    def loop[A](source: CaioT[A], n: Int): CaioT[A] =
      source.attempt.flatMap {
        case Right(l) =>
          if (n <= 0) Caio.pure(l)
          else loop(source, n - 1)
        case Left(e)  =>
          Caio.raiseError(e)
      }

    val f = params.CE.unsafeToFuture(loop(Caio("value"), 10000))

    params.EC.tick()

    assertEquals(f.value, Some(Success("value")))
  }

  testAsync("attempt foldLeft sequence") { params =>
    val count = 10000
    val loop  = (0 until count).foldLeft(Caio(0)) { (acc, _) =>
      acc.attempt.flatMap {
        case Right(x) => Caio.pure(x + 1)
        case Left(e)  => Caio.raiseError(e)
      }
    }

    val f = params.CE.unsafeToFuture(loop)

    params.EC.tick()

    assertEquals(f.value, Some(Success(count)))
  }

  testAsync("Caio(throw ex).attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio  = Caio[Int](throw dummy).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("Caio(throw ex).flatMap.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio  = Caio[Int](throw dummy).flatMap(Caio.pure).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("Caio(throw ex).map.attempt.map") { params =>
    val dummy = new RuntimeException("dummy")
    val caio  = Caio[Int](throw dummy).map(x => x).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.attempt.map") { params =>
    val dummy  = new RuntimeException("dummy")
    val source = Caio.async_[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.flatMap.attempt.map") { params =>
    val dummy  = new RuntimeException("dummy")
    val source = Caio.async_[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.flatMap(Caio.pure).attempt.map {
      case Left(`dummy`) => 100
      case _             => 0
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("IO.async.attempt.flatMap") { params =>
    val dummy  = new RuntimeException("dummy")
    val source = Caio.async_[Int] { callback =>
      params.EC.execute(() => callback(Left(dummy)))
    }

    val caio = source.attempt.flatMap {
      case Left(`dummy`) => Caio.pure(100)
      case _             => Caio.pure(0)
    }

    val f = params.CE.unsafeToFuture(caio)

    params.EC.tick()

    assertEquals(f.value, Some(Success(100)))
  }

  testAsync("bracket signals the error in use") { params =>
    val e = new RuntimeException("error in use")

    val caio = Caio.unit
      .bracket[C, L, Unit, Unit](_ => Caio.raiseError(e))(_ => Caio.unit)
      .attempt

    val r = params.CE.unsafeRunSync(caio)

    assertEquals(r, Left(e))
    assert(e.getSuppressed.isEmpty)
  }

  testAsync("bracket signals the error in release") { params =>
    val e = new RuntimeException("error in release")

    val caio = Caio.unit
      .bracket[C, L, Unit, Unit](_ => Caio.unit)(_ => Caio.raiseError(e))
      .attempt

    val r = params.CE.unsafeRunSync(caio)

    assertEquals(r, Left(e))
    assert(e.getSuppressed.isEmpty)
  }

  testGlobalAsync("bracket signals the error in use and logs the error from release") { params =>
    val e1 = new RuntimeException("error in use")
    val e2 = new RuntimeException("error in release")

    var r: Option[Either[Throwable, Nothing]] = None
    val sysErr                                = catchSystemErr {
      val caio =
        Caio.unit
          .bracket[C, L, Unit, Nothing](_ => Caio.raiseError(e1))(_ => Caio.raiseError(e2))
          .attempt

      r = Some(params.CE.unsafeRunSync(caio))
    }

    assertEquals(r, Some(Left(e1)))
    assert(sysErr.contains("error in release"))
    assert(e1.getSuppressed.isEmpty)
    assert(e2.getSuppressed.isEmpty)
  }

  testAsync("bracket does not evaluate use on cancel") { params =>
    import params._

    var use     = false
    var release = false

    val task = Caio
      .sleep(2.second)
      .bracket[C, L, Unit, Unit](_ => Caio { use = true })(_ => Caio { release = true })
      .timeoutTo[C, L, Unit](1.second, Caio.never)

    val f = CE.unsafeToFuture(task)

    EC.tick()

    assertEquals(f.value, None)
    assertEquals(use, false)
    assertEquals(release, false)
  }

  testAsync("start forks automatically") { params =>
    import params._
    val value = RealCE.unsafeRunSync(Caio(1).start.flatMap(_.joinWith(Caio.pure(0))))
    assertEquals(value, 1)
  }

  testAsync("background cancels the action in cleanup") { params =>
    import params._

    val caio: CaioT[OutcomeCaio[C, L, Unit]] =
      Deferred[CaioT, Unit].flatMap { started =>
        Deferred[CaioT, OutcomeCaio[C, L, Unit]].flatMap { result =>
          val bg: CaioT[Unit] =
            started.complete(()) *> Caio.never[Unit].guaranteeCase[C, L, Unit](result.complete(_).void)

          bg.background.use[Unit](_ => started.get) *> result.get
        }
      }

    val f = CE.unsafeToFuture(caio)

    EC.tick()

    assertEquals(f.value, Some(Success(Outcome.canceled[CaioT, Throwable, Unit])))
  }

  testAsync("background allows awaiting the action") { params =>
    import params._

    val caio = Deferred[CaioT, Unit]
      .flatMap { latch =>
        val bg = latch.get *> Caio.pure(42)

        bg.background.use((await: CaioT[OutcomeCaio[C, L, Int]]) => latch.complete(()) *> await)
      }

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, Outcome.succeeded[CaioT, Throwable, Int](Caio.pure(42)))
  }

  testAsync("cancel should wait for already started finalizers on success") { params =>
    import params._

    val caio = for {
      pa    <- Deferred[CaioT, Unit]
      fiber <- Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(200.millis)).start
      _     <- pa.get
      _     <- fiber.cancel
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }

  testAsync("cancel should wait for already started finalizers on failure") { params =>
    import params._

    val dummy = new RuntimeException("dummy")

    val caio = for {
      pa    <- Deferred[CaioT, Unit]
      fiber <- Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(200.millis) *> Caio.raiseError(dummy)).start
      _     <- pa.get
      _     <- fiber.cancel
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }

  testAsync("cancel should wait for already started use finalizers") { params =>
    import params._

    val caio = for {
      pa   <- Deferred[CaioT, Unit]
      fibA <-
        Caio.unit
          .bracket[C, L, Unit, Unit](_ => Caio.unit.guarantee(pa.complete(()) *> Caio.sleep(200.millis)))(_ => Caio.unit)
          .start
      _    <- pa.get
      _    <- fibA.cancel
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }

  testAsync("second cancel should wait for use finalizers") { params =>
    import params._

    val caio = for {
      pa    <- Deferred[CaioT, Unit]
      fiber <- Caio.unit
                 .bracket[C, L, Unit, Unit](_ => (pa.complete(()) *> Caio.never).guarantee(Caio.sleep(200.millis)))(_ =>
                   Caio.unit
                 )
                 .start
      _     <- pa.get
      _     <- Caio.race(fiber.cancel, fiber.cancel)
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }

  testAsync("second cancel during acquire should wait for it and finalizers to complete") { params =>
    import params._

    val caio = for {
      pa    <- Deferred[CaioT, Unit]
      fiber <- (pa.complete(()) *> Caio.sleep(200.millis))
                 .bracket[C, L, Unit, Unit](_ => Caio.unit)(_ => Caio.sleep(200.millis))
                 .start
      _     <- pa.get
      _     <- Caio.race(fiber.cancel, fiber.cancel)
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }

  testAsync("Multiple cancel should not hang") { params =>
    import params._

    val sleep =
      Caio.sleep(1.second)

    val caio = for {
      fiber <- sleep.start
      _     <- fiber.cancel
      _     <- fiber.cancel
    } yield ()

    val value = RealCE.unsafeRunSync(caio)

    assertEquals(value, ())
  }
}
