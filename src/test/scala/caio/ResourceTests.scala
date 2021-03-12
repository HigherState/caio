package caio

import java.util.concurrent.atomic.AtomicBoolean

import cats.data.Kleisli
import cats.{MonadError, ~>}
import cats.effect.concurrent.Deferred
import cats.effect.laws.util.TestContext
import cats.effect.{Blocker, ExitCase, IO, Resource}
import cats.effect.Resource._
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxApply, catsSyntaxSemigroup, catsSyntaxTuple2Parallel, toFunctorOps, toTraverseOps}
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.IsEqArrow
import cats.laws.discipline.{CommutativeApplicativeTests, MonadErrorTests, ParallelTests, SemigroupKTests, catsLawsIsEqToProp}
import cats.syntax.applicativeError._
import cats.syntax.semigroupk._
import org.scalacheck.Prop.forAll

import scala.concurrent.duration.DurationInt
import scala.util.Success

class ResourceTests extends TestInstances {
  import arbitrary._
  import cats.laws.discipline.arbitrary._
  import cats.effect.laws.discipline.arbitrary._

  checkAllAsync("Resource[Caio, *]"){ params =>
    import params._
    MonadErrorTests[Resource[CaioT, ?], Throwable].monadError[Int, Int, Int]
  }

  checkAllAsync("Resource[Caio, Int]"){ params =>
    import params._
    MonoidTests[Resource[CaioT, Int]].monoid
  }

  checkAllAsync("Resource[Caio, *]"){ params =>
    import params._
    SemigroupKTests[Resource[CaioT, ?]].semigroupK[Int]
  }

  checkAllAsync("Resource.Par[Caio, *]") { params =>
    import params._
    import implicits.dynamicCaioParallel
    CommutativeApplicativeTests[Resource.Par[CaioT, ?]].commutativeApplicative[Int, Int, Int]
  }

  checkAllAsync("Resource[Caio, *]") { params =>
    import params._
    import implicits.dynamicCaioParallel
    val module = ParallelTests[Resource[CaioT, *]]
    module.parallel[Int, Int]
  }

  testAsync("Resource.make is equivalent to a partially applied bracket") { params =>
    import params._
    forAll { (acquire: CaioT[String], release: String => CaioT[Unit], f: String => CaioT[String]) =>
      acquire.bracket(f)(release) <-> Resource.make(acquire)(release).use(f)
    }
  }

  testAsync("releases resources in reverse order of acquisition") { params =>
    import params._

    forAll { (as: List[(Int, Either[Throwable, Unit])]) =>
      var released: List[Int] = Nil

      val r: Resource[CaioT, List[Int]] = as.traverse {
        case (a, e) =>
          Resource.make[CaioT, Int](Caio(a))(a => Caio { released = a :: released } *> Caio.fromEither(e))
      }

      val caio = r.use(Caio.pure).attempt
      CE.toIO(caio).unsafeRunSync()
      released <-> as.map(_._1)
    }
  }

  testAsync("releases both resources on combine") { params =>
    import params._

    forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
      var acquired: Set[Int] = Set.empty
      var released: Set[Int] = Set.empty
      def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
        Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
      }
      val caio = observe(rx).combine(observe(ry)).use(_ => Caio.unit).attempt
      CE.toIO(caio).unsafeRunSync()
      released <-> acquired
    }
  }

  testAsync("releases both resources on combineK") { params =>
    import params._
    forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
      var acquired: Set[Int] = Set.empty
      var released: Set[Int] = Set.empty
      def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
        Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
      }
      val caio = observe(rx).combineK(observe(ry)).use(_ => Caio.unit).attempt
      CE.toIO(caio).unsafeRunSync()
      released <-> acquired
    }
  }

  testAsync("releases both resources on combineK when using a SemigroupK instance that discards allocated values") { params =>
    import params._
    forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
      var acquired: Set[Int] = Set.empty
      var released: Set[Int] = Set.empty
      def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
        Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
      }
      val caio = observe(rx).combineK(observe(ry)).use(_ => Caio.unit).attempt
      CE.toIO(caio).unsafeRunSync()
      released <-> acquired
    }
  }

  testGlobalAsync("resource from AutoCloseable is auto closed") { params =>
    import params._

    var closed = false
    val autoCloseable = new AutoCloseable {
      override def close(): Unit = closed = true
    }

    val caio = Resource
      .fromAutoCloseable[CaioT, AutoCloseable](Caio(autoCloseable))
      .use(_ => Caio.pure("Hello world"))

    val result = CE.toIO(caio).unsafeRunSync()

    assertEquals(result, "Hello world")
    assertEquals(closed, true)
  }

  testAsync("resource from AutoCloseableBlocking is auto closed and executes in the blocking context") { params =>
    import params._

    implicit val contextShift = params.EC.contextShift[CaioT]

    val blockingEc = TestContext()

    val blocker = Blocker.liftExecutionContext(blockingEc)

    var closed = false
    val autoCloseable = new AutoCloseable {
      override def close(): Unit = closed = true
    }

    var acquired = false
    val acquire: CaioT[AutoCloseable] = Caio {
      acquired = true
      autoCloseable
    }

    val caio = Resource
      .fromAutoCloseableBlocking(blocker)(acquire)
      .use(_ => Caio.pure("Hello world"))

    val result = CE.toIO(caio).unsafeToFuture()

    // Check that acquire ran inside the blocking context.
    EC.tick()
    assertEquals(acquired, false)
    blockingEc.tick()
    assertEquals(acquired, true)

    // Check that close was called and ran inside the blocking context.
    EC.tick()
    assertEquals(closed, false)
    blockingEc.tick()
    assertEquals(closed, true)

    // Check the final result.
    EC.tick()
    assertEquals(result.value, Some(Success("Hello world")))
  }

  testAsync("liftF") { params =>
    import params._

    forAll { (fa: CaioT[String]) =>
      Resource.liftF(fa).use(Caio.pure) <-> fa
    }
  }

  testAsync("liftF - interruption") { params =>
    import params._

    implicit val timer = getTimer(params.EC.timer[IO])

    def p =
      Deferred[CaioT, ExitCase[Throwable]]
        .flatMap { stop =>
          val r = Resource
            .liftF(Caio.never: CaioT[Int])
            .use(Caio.pure)
            .guaranteeCase(stop.complete)

          r.start[C, V, L, Int].flatMap { fiber =>
            timer.sleep(200.millis) *> fiber.cancel *> stop.get
          }
        }
        .timeout(2.seconds)

    val res = CE.toIO(p).unsafeToFuture()

    EC.tick(3.seconds)

    assertEquals(res.value, Some(Success(ExitCase.Canceled)))
  }

  testAsync("liftF(fa) <-> liftK.apply(fa)") { params =>
    import params._

    forAll { (fa: CaioT[String], f: String => CaioT[Int]) =>
      Resource.liftF(fa).use(f) <-> Resource.liftK[CaioT].apply(fa).use(f)
    }
  }

  testAsync("evalMap") { params =>
    import params._

    forAll { (f: Int => CaioT[Int]) =>
      Resource.liftF[CaioT, Int](Caio(0)).evalMap(f).use(Caio.pure) <-> f(0)
    }
  }

  testAsync("(evalMap with error <-> Caio.raiseError") { params =>
    import params._

    case object Foo extends Exception

    forAll { (g: Int => CaioT[Int]) =>
      val effect: Int => CaioT[Int] = a => g(a) <* Caio(throw Foo)
      Resource.liftF[CaioT, Int](Caio(0)).evalMap(effect).use(Caio.pure) <-> Caio.raiseError(Foo)
    }
  }

  testAsync("evalTap") { params =>
    import params._

    forAll { (f: Int => CaioT[Int]) =>
      Resource.liftF[CaioT, Int](Caio(0)).evalTap(f).use(Caio.pure) <-> f(0).as(0)
    }
  }

  /*testAsync("evalTap with cancellation <-> IO.never") { params =>
    import params._

    forAll { (g: Int => CaioT[Int]) =>
      val effect: Int => CaioT[Int] = a =>
        for {
          f <- (g(a) <* IO.cancelBoundary).start
          _ <- f.cancel
          r <- f.join
        } yield r

      Resource.liftF(IO(0)).evalTap(effect).use(IO.pure) <-> IO.never
    }
  }*/

  testAsync("(evalTap with error <-> Caio.raiseError") { params =>
    import params._
    case object Foo extends Exception

    forAll { (g: Int => CaioT[Int]) =>
      val effect: Int => CaioT[Int] = a => (g(a) <* Caio(throw Foo))
      Resource.liftF[CaioT, Int](Caio(0)).evalTap(effect).use(Caio.pure) <-> Caio.raiseError(Foo)
    }
  }

  testAsync("mapK") { params =>
    import params._

    forAll { (fa: Kleisli[CaioT, Int, Int]) =>
      val runWithTwo = new ~>[Kleisli[CaioT, Int, *], CaioT] {
        override def apply[A](fa: Kleisli[CaioT, Int, A]): CaioT[A] = fa(2)
      }
      Resource.liftF[Kleisli[CaioT, Int, ?], Int](fa).mapK(runWithTwo).use(Caio.pure) <-> fa(2)
    }
  }

  testGlobalAsync("mapK should preserve ExitCode-specific behaviour") { params =>
    import params._

    val takeAnInteger = new ~>[CaioT, Kleisli[CaioT, Int, *]] {
      override def apply[A](fa: CaioT[A]): Kleisli[CaioT, Int, A] = Kleisli.liftF(fa)
    }

    def sideEffectyResource: (AtomicBoolean, Resource[CaioT, Unit]) = {
      val cleanExit = new java.util.concurrent.atomic.AtomicBoolean(false)
      val res = Resource.makeCase[CaioT, Unit](Caio.unit) {
        case (_, ExitCase.Completed) =>
          Caio {
            cleanExit.set(true)
          }
        case _ => Caio.unit
      }
      (cleanExit, res)
    }

    val (clean, res) = sideEffectyResource
    CE.toIO(res.use(_ => Caio.unit).attempt).unsafeRunSync()
    assertEquals(clean.get(), true)

    val (clean1, res1) = sideEffectyResource
    CE.toIO(res1.use(_ => Caio.raiseError(new Throwable("oh no"))).attempt).unsafeRunSync()
    assertEquals(clean1.get(), false)

    val (clean2, res2) = sideEffectyResource
    val caio =
      res2
        .mapK(takeAnInteger)
        .use(_ => Kleisli.liftF[CaioT, Int, Unit](Caio.raiseError(new Throwable("oh no"))))
        .run(0)
        .attempt

    CE.toIO(caio).unsafeRunSync()
    assertEquals(clean2.get(), false)
  }

  testAsync("allocated produces the same value as the resource") { params =>
    import params._

    forAll { (resource: Resource[CaioT, Int]) =>
      val a0 = Resource(resource.allocated).use(Caio.pure).attempt
      val a1 = resource.use(Caio.pure).attempt

      a0 <-> a1
    }
  }

  testGlobalAsync("allocate does not release until close is invoked") { params =>
    import params._

    val released = new java.util.concurrent.atomic.AtomicBoolean(false)
    val release = Resource.make[CaioT, Unit](Caio.unit)(_ => Caio(released.set(true)))
    val resource = Resource.liftF[CaioT, Unit](Caio.unit)

    val allocated = (release *> resource).allocated

    val prog = for {
      res <- allocated
      (_, close) = res
      _ <- Caio(assertEquals(released.get(), false))
      _ <- close
      _ <- Caio(assertEquals(released.get(), true))
    } yield ()

    CE.toIO(prog).unsafeRunSync()
  }

  testGlobalAsync("allocate does not release until close is invoked on mapK'd Resources") { params =>
    import params._

    val released = new java.util.concurrent.atomic.AtomicBoolean(false)

    val runWithTwo = new ~>[Kleisli[CaioT, Int, *], CaioT] {
      override def apply[A](fa: Kleisli[CaioT, Int, A]): CaioT[A] = fa(2)
    }
    val takeAnInteger = new ~>[CaioT, Kleisli[CaioT, Int, *]] {
      override def apply[A](fa: CaioT[A]): Kleisli[CaioT, Int, A] = Kleisli.liftF(fa)
    }
    val plusOne: Kleisli[CaioT, Int, Int] = Kleisli { (i: Int) =>
      Caio(i + 1)
    }
    val plusOneResource = Resource.liftF[Kleisli[CaioT, Int, ?], Int](plusOne)

    val release = Resource.make[CaioT, Unit](Caio.unit)(_ => Caio(released.set(true)))
    val resource = Resource.liftF[CaioT, Unit](Caio.unit)

    // Dotty fails to infer functor syntax if this line is in the for comprehension
    val allocated = ((release *> resource).mapK(takeAnInteger) *> plusOneResource).mapK(runWithTwo).allocated

    val prog = for {
      res <- allocated
      (_, close) = res
      _ <- Caio(assertEquals(released.get(), false))
      _ <- close
      _ <- Caio(assertEquals(released.get(), true))
    } yield ()

    CE.toIO(prog).unsafeRunSync()
  }

  testGlobalAsync("safe attempt suspended resource") { params =>
    import params._
    val exception = new Exception("boom!")
    val suspend = Resource.suspend[CaioT, Int](Caio.raiseError(exception))
    val attempt = MonadError[Resource[CaioT, *], Throwable].attempt(suspend)
    assertEquals(CE.toIO(attempt.use(Caio.pure)).unsafeRunSync(), Left(exception))
  }

  testGlobalAsync("combineK - should behave like orElse when underlying effect does") { params =>
    import params._

    forAll { (r1: Resource[CaioT, Int], r2: Resource[CaioT, Int]) =>
      val lhs = CE.toIO(r1.orElse(r2).use(Caio.pure).attempt).unsafeRunSync()
      val rhs = CE.toIO((r1 <+> r2).use(Caio.pure).attempt).unsafeRunSync()

      lhs <-> rhs
    }
  }

  testGlobalAsync("combineK - should behave like underlying effect") { params =>
    import params._
    import cats.data.OptionT
    forAll { (ot1: OptionT[CaioT, Int], ot2: OptionT[CaioT, Int]) =>
      val lhs: Either[Throwable, Option[Int]] =
        CE.toIO(Resource.liftF[OptionT[CaioT, *], Int](ot1 <+> ot2).use(OptionT.pure[CaioT](_)).value.attempt).unsafeRunSync()
      val rhs: Either[Throwable, Option[Int]] =
        CE.toIO(
          (Resource.liftF[OptionT[CaioT, *], Int](ot1) <+> Resource.liftF[OptionT[CaioT, *], Int](ot2))
            .use(OptionT.pure[CaioT](_))
            .value
            .attempt
        ).unsafeRunSync()

      lhs <-> rhs
    }
  }

  testAsync("parZip - releases resources in reverse order of acquisition") { params =>
    import params._
    implicit val parallel = implicits.dynamicCaioParallel[C]

    forAll { (as: List[(Int, Either[Throwable, Unit])], rhs: Boolean) =>
      var released: List[Int] = Nil
      val r = as.traverse {
        case (a, e) =>
          Resource.make[CaioT, Int](Caio(a))(a => Caio { released = a :: released } *> Caio.fromEither(e))
      }
      val unit = ().pure[Resource[CaioT, *]]
      val p = if (rhs) r.parZip(unit) else unit.parZip(r)

      CE.toIO(p.use(Caio.pure).attempt).unsafeToFuture()
      EC.tick()
      released <-> as.map(_._1)
    }
  }

  testAsync("parZip - parallel acquisition and release") { params =>
    import params._
    implicit val timer = getTimer(params.EC.timer[IO])
    implicit val parallel = implicits.dynamicCaioParallel[C]

    var leftAllocated = false
    var rightAllocated = false
    var leftReleasing = false
    var rightReleasing = false
    var leftReleased = false
    var rightReleased = false

    val wait = Caio.sleep(1.second)
    val lhs = Resource.make[CaioT, Unit](wait *> Caio { leftAllocated = true }) { _ =>
      Caio { leftReleasing = true } *> wait *> Caio { leftReleased = true }
    }
    val rhs = Resource.make[CaioT, Unit](wait *> Caio { rightAllocated = true }) { _ =>
      Caio { rightReleasing = true } *> wait *> Caio { rightReleased = true }
    }

    CE.toIO((lhs, rhs).parTupled.use(_ => wait)).unsafeToFuture()

    EC.tick(1.second)
    assertEquals(leftAllocated, true)
    assertEquals(rightAllocated, true)
    assertEquals(leftReleasing, false)
    assertEquals(rightReleasing, false)

    EC.tick(1.second)
    assertEquals(leftReleasing, true)
    assertEquals(rightReleasing, true)
    assertEquals(leftReleased, false)
    assertEquals(rightReleased, false)

    EC.tick(1.second)
    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("parZip - safety: lhs error during rhs interruptible region") { params =>
    import params._
    implicit val timer = getTimer(params.EC.timer[IO])
    implicit val parallel = implicits.dynamicCaioParallel[C]

    var leftAllocated = false
    var rightAllocated = false
    var leftReleasing = false
    var rightReleasing = false
    var leftReleased = false
    var rightReleased = false

    def wait(n: Int): CaioT[Unit] = Caio.sleep(n.seconds)
    val lhs = for {
      _ <- Resource.make(wait(1) *> Caio { leftAllocated = true }) { _ =>
        Caio { leftReleasing = true } *> wait(1) *> Caio { leftReleased = true }
      }
      _ <- Resource.liftF[CaioT, Unit](wait(1) *> Caio.raiseError(new Exception))
    } yield ()

    val rhs = for {
      _ <- Resource.make(wait(1) *> Caio { rightAllocated = true }) { _ =>
        Caio { rightReleasing = true } *> wait(1) *> Caio { rightReleased = true }
      }
      _ <- Resource.liftF(wait(2))
    } yield ()

    val caio =
      (lhs, rhs).parTupled
        .use(_ => Caio.unit)
        .handleError(_ => ())

    CE.toIO(caio).unsafeToFuture()

    EC.tick(1.second)
    assertEquals(leftAllocated, true)
    assertEquals(rightAllocated, true)
    assertEquals(leftReleasing, false)
    assertEquals(rightReleasing, false)

    EC.tick(1.second)
    assertEquals(leftReleasing, true)
    assertEquals(rightReleasing, true)
    assertEquals(leftReleased, false)
    assertEquals(rightReleased, false)

    EC.tick(1.second)
    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("parZip - safety: rhs error during lhs uninterruptible region") { params =>
    import params._
    implicit val timer = getTimer(params.EC.timer[IO])
    implicit val parallel = implicits.dynamicCaioParallel[C]

    var leftAllocated = false
    var rightAllocated = false
    var rightErrored = false
    var leftReleasing = false
    var rightReleasing = false
    var leftReleased = false
    var rightReleased = false

    def wait(n: Int): CaioT[Unit] = Caio.sleep(n.seconds)
    val lhs = Resource.make(wait(3) *> Caio { leftAllocated = true }) { _ =>
      Caio { leftReleasing = true } *> wait(1) *> Caio { leftReleased = true }
    }
    val rhs = for {
      _ <- Resource.make[CaioT, Unit](wait(1) *> Caio { rightAllocated = true }) { _ =>
        Caio { rightReleasing = true } *> wait(1) *> Caio { rightReleased = true }
      }
      _ <- Resource.make[CaioT, Unit](wait(1) *> Caio { rightErrored = true } *> Caio.raiseError(new Exception))(_ => Caio.unit)
    } yield ()

    val caio =
      (lhs, rhs).parTupled
        .use(_ => wait(1))
        .handleError(_ => ())

    CE.toIO(caio).unsafeToFuture()

    EC.tick(1.second)
    assertEquals(leftAllocated, false)
    assertEquals(rightAllocated, true)
    assertEquals(rightErrored, false)
    assertEquals(leftReleasing, false)
    assertEquals(rightReleasing, false)

    EC.tick(1.second)
    assertEquals(leftAllocated, false)
    assertEquals(rightAllocated, true)
    assertEquals(rightErrored, true)
    assertEquals(leftReleasing, false)
    assertEquals(rightReleasing, false)

    EC.tick(1.second)
    assertEquals(leftAllocated, true)
    assertEquals(leftReleasing, true)
    assertEquals(rightReleasing, true)
    assertEquals(leftReleased, false)
    assertEquals(rightReleased, false)

    EC.tick(1.second)
    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("onFinalizeCase - interruption") { params =>
    import params._
    implicit val timer = getTimer(params.EC.timer[IO])

    def p: CaioT[ExitCase[Throwable]] =
      Deferred[CaioT, ExitCase[Throwable]]
        .flatMap { stop =>
          val r = Resource
            .liftF(Caio.never: CaioT[Int])
            .onFinalizeCase(stop.complete)
            .use(Caio.pure)

          r.start[C, V, L, Int].flatMap { fiber =>
            timer.sleep(200.millis) *> fiber.cancel *> stop.get
          }
        }
        .timeout(2.seconds)

    val res = CE.toIO(p).unsafeToFuture()

    EC.tick(3.seconds)

    assertEquals(res.value, Some(Success(ExitCase.Canceled)))
  }

  testAsync("onFinalize - runs after existing finalizer") { params =>
    import params._

    forAll { (rx: Resource[CaioT, Int], y: Int) =>
      var acquired: List[Int] = Nil
      var released: List[Int] = Nil

      def observe(r: Resource[CaioT, Int]): Resource[CaioT, Unit] =
        r.flatMap { a =>
          Resource
            .make[CaioT, Int](Caio {
              acquired = a :: acquired
            } *> Caio.pure(a))(a =>
              Caio {
                released = a :: released
              }
            )
            .as(())
        }

      val caio =
        observe(rx)
          .onFinalize(Caio {
            released = y :: released
          })
          .use(_ => Caio.unit)
          .attempt

      CE.toIO(caio).unsafeRunSync()

      released <-> y :: acquired
    }
  }
}
