package caio

import java.util.concurrent.atomic.AtomicBoolean

import cats.data.Kleisli
import cats.{~>, MonadError}
import cats.effect.{Deferred, Outcome, Resource}
import cats.effect.Resource._
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.IsEqArrow
import cats.laws.discipline.{catsLawsIsEqToProp, MonadErrorTests, ParallelTests, SemigroupKTests}
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.functor._
import cats.syntax.semigroupk._
import cats.syntax.traverse._
import cats.syntax.parallel._
import org.scalacheck.Prop.forAll

import scala.concurrent.duration.DurationInt

class ResourceTests extends TestInstances {
  import arbitrary._
  import cats.laws.discipline.arbitrary._

  checkAllAsync("Resource[Caio, *]") { params =>
    import params._
    MonadErrorTests[Resource[CaioT, *], Throwable].monadError[Int, Int, Int]
  }

  checkAllAsync("Resource[Caio, Int]") { params =>
    import params._
    MonoidTests[Resource[CaioT, Int]].monoid
  }

  checkAllAsync("Resource[Caio, *]") { params =>
    import params._
    SemigroupKTests[Resource[CaioT, *]].semigroupK[Int]
  }

  testAsync("Resource.make is equivalent to a partially applied bracket") { params =>
    import params._
    forAll { (acquire: CaioT[String], release: String => CaioT[Unit], f: String => CaioT[String]) =>
      acquire.bracket(f)(release) <-> Resource.make(acquire)(release).use(f)
    }
  }

  checkAllAsync("Resource[Caio, *]") { params =>
    import params._
    val module = ParallelTests[Resource[CaioT, *]]
    module.parallel[Int, Int]
  }

  testAsync("releases resources in reverse order of acquisition") { params =>
    import params._

    forAll { (as: List[(Int, Either[Throwable, Unit])]) =>
      var released: List[Int] = Nil

      val r: Resource[CaioT, List[Int]] = as.traverse { case (a, e) =>
        Resource.make[CaioT, Int](Caio(a))(a => Caio { released = a :: released } *> Caio.fromEither(e))
      }

      val caio = r.use(Caio.pure).attempt
      CE.unsafeRunSync(caio)
      released <-> as.map(_._1)
    }
  }

  testAsync("releases both resources on combine") { params =>
    import params._

    forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
      var acquired: Set[Int]               = Set.empty
      var released: Set[Int]               = Set.empty
      def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
        Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
      }
      val caio                             = observe(rx).combine(observe(ry)).use(_ => Caio.unit).attempt
      CE.unsafeRunSync(caio)
      released <-> acquired
    }
  }

  testAsync("releases both resources on combineK") { params =>
    import params._
    forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
      var acquired: Set[Int]               = Set.empty
      var released: Set[Int]               = Set.empty
      def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
        Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
      }
      val caio                             = observe(rx).combineK(observe(ry)).use(_ => Caio.unit).attempt
      CE.unsafeRunSync(caio)
      released <-> acquired
    }
  }

  testAsync("releases both resources on combineK when using a SemigroupK instance that discards allocated values") {
    params =>
      import params._
      forAll { (rx: Resource[CaioT, Int], ry: Resource[CaioT, Int]) =>
        var acquired: Set[Int]               = Set.empty
        var released: Set[Int]               = Set.empty
        def observe(r: Resource[CaioT, Int]) = r.flatMap { a =>
          Resource.make[CaioT, Int](Caio(acquired += a) *> Caio.pure(a))(a => Caio(released += a)).as(())
        }
        val caio                             = observe(rx).combineK(observe(ry)).use(_ => Caio.unit).attempt
        CE.unsafeRunSync(caio)
        released <-> acquired
      }
  }

  testGlobalAsync("resource from AutoCloseable is auto closed") { params =>
    import params._

    var closed        = false
    val autoCloseable = new AutoCloseable {
      override def close(): Unit = closed = true
    }

    val caio = Resource
      .fromAutoCloseable[CaioT, AutoCloseable](Caio(autoCloseable))
      .use(_ => Caio.pure("Hello world"))

    val result = RealCE.unsafeRunSync(caio)

    assertEquals(result, "Hello world")
    assertEquals(closed, true)
  }

  testAsync("eval") { params =>
    import params._

    forAll { (fa: CaioT[String]) =>
      Resource.eval(fa).use(Caio.pure) <-> fa
    }
  }

  testAsync("eval - interruption") { params =>
    import params._

    def p =
      Deferred[CaioT, OutcomeCaio[C, L, Int]]
        .flatMap { stop =>
          val r = Resource
            .eval(Caio.never: CaioT[Int])
            .use(Caio.pure)
            .guaranteeCase[C, L, Int](stop.complete(_).void)

          r.start.flatMap { fiber =>
            Caio.sleep(200.millis) *> fiber.cancel *> stop.get
          }
        }
        .timeout(2.seconds)

    val value = RealCE.unsafeRunSync(p)

    assertEquals(value, Outcome.canceled[CaioT, Throwable, Int])
  }

  testAsync("eval(fa) <-> eval.apply(fa)") { params =>
    import params._

    forAll { (fa: CaioT[String], f: String => CaioT[Int]) =>
      Resource.eval(fa).use(f) <-> Resource.liftK[CaioT].apply(fa).use(f)
    }
  }

  testAsync("evalMap") { params =>
    import params._

    forAll { (f: Int => CaioT[Int]) =>
      Resource.eval[CaioT, Int](Caio(0)).evalMap(f).use(Caio.pure) <-> f(0)
    }
  }

  testAsync("(evalMap with error <-> Caio.raiseError") { params =>
    import params._

    case object Foo extends Exception

    forAll { (g: Int => CaioT[Int]) =>
      val effect: Int => CaioT[Int] = a => g(a) <* Caio(throw Foo)
      Resource.eval[CaioT, Int](Caio(0)).evalMap(effect).use(Caio.pure) <-> Caio.raiseError(Foo)
    }
  }

  testAsync("evalTap") { params =>
    import params._

    forAll { (f: Int => CaioT[Int]) =>
      Resource.eval[CaioT, Int](Caio(0)).evalTap(f).use(Caio.pure) <-> f(0).as(0)
    }
  }

  testAsync("(evalTap with error <-> Caio.raiseError") { params =>
    import params._
    case object Foo extends Exception

    forAll { (g: Int => CaioT[Int]) =>
      val effect: Int => CaioT[Int] = a => (g(a) <* Caio(throw Foo))
      Resource.eval[CaioT, Int](Caio(0)).evalTap(effect).use(Caio.pure) <-> Caio.raiseError(Foo)
    }
  }

  testAsync("mapK") { params =>
    import params._

    forAll { (fa: Kleisli[CaioT, Int, Int]) =>
      val runWithTwo = new ~>[Kleisli[CaioT, Int, *], CaioT] {
        override def apply[A](fa: Kleisli[CaioT, Int, A]): CaioT[A] = fa(2)
      }
      Resource.eval[Kleisli[CaioT, Int, *], Int](fa).mapK(runWithTwo).use(Caio.pure) <-> fa(2)
    }
  }

  testGlobalAsync("mapK should preserve ExitCode-specific behaviour") { params =>
    import params._

    val takeAnInteger = new ~>[CaioT, Kleisli[CaioT, Int, *]] {
      override def apply[A](fa: CaioT[A]): Kleisli[CaioT, Int, A] = Kleisli.liftF(fa)
    }

    def sideEffectyResource: (AtomicBoolean, Resource[CaioT, Unit]) = {
      val cleanExit = new java.util.concurrent.atomic.AtomicBoolean(false)
      val res       = Resource.makeCase[CaioT, Unit](Caio.unit) {
        case (_, ExitCase.Succeeded) =>
          Caio {
            cleanExit.set(true)
          }
        case _                       => Caio.unit
      }
      (cleanExit, res)
    }

    val (clean, res) = sideEffectyResource
    CE.unsafeRunSync(res.use(_ => Caio.unit).attempt)
    assertEquals(clean.get(), true)

    val (clean1, res1) = sideEffectyResource
    CE.unsafeRunSync(res1.use(_ => Caio.raiseError(new Throwable("oh no"))).attempt)
    assertEquals(clean1.get(), false)

    val (clean2, res2) = sideEffectyResource
    val caio           =
      res2
        .mapK(takeAnInteger)
        .use(_ => Kleisli.liftF[CaioT, Int, Unit](Caio.raiseError(new Throwable("oh no"))))
        .run(0)
        .attempt

    CE.unsafeRunSync(caio)
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
    val release  = Resource.make[CaioT, Unit](Caio.unit)(_ => Caio(released.set(true)))
    val resource = Resource.eval[CaioT, Unit](Caio.unit)

    val allocated = (release *> resource).allocated

    val prog = for {
      res       <- allocated
      (_, close) = res
      _         <- Caio(assertEquals(released.get(), false))
      _         <- close
      _         <- Caio(assertEquals(released.get(), true))
    } yield ()

    CE.unsafeRunSync(prog)
  }

  testGlobalAsync("allocate does not release until close is invoked on mapK'd Resources") { params =>
    import params._

    val released = new java.util.concurrent.atomic.AtomicBoolean(false)

    val runWithTwo                        = new ~>[Kleisli[CaioT, Int, *], CaioT] {
      override def apply[A](fa: Kleisli[CaioT, Int, A]): CaioT[A] = fa(2)
    }
    val takeAnInteger                     = new ~>[CaioT, Kleisli[CaioT, Int, *]] {
      override def apply[A](fa: CaioT[A]): Kleisli[CaioT, Int, A] = Kleisli.liftF(fa)
    }
    val plusOne: Kleisli[CaioT, Int, Int] = Kleisli { (i: Int) =>
      Caio(i + 1)
    }
    val plusOneResource                   = Resource.eval[Kleisli[CaioT, Int, *], Int](plusOne)

    val release  = Resource.make[CaioT, Unit](Caio.unit)(_ => Caio(released.set(true)))
    val resource = Resource.eval[CaioT, Unit](Caio.unit)

    val allocated = ((release *> resource).mapK(takeAnInteger) *> plusOneResource).mapK(runWithTwo).allocated

    val prog = for {
      res       <- allocated
      (_, close) = res
      _         <- Caio(assertEquals(released.get(), false))
      _         <- close
      _         <- Caio(assertEquals(released.get(), true))
    } yield ()

    CE.unsafeRunSync(prog)
  }

  testGlobalAsync("safe attempt suspended resource") { params =>
    import params._
    val exception = new Exception("boom!")
    val suspend   = Resource.suspend[CaioT, Int](Caio.raiseError(exception))
    val attempt   = MonadError[Resource[CaioT, *], Throwable].attempt(suspend)
    assertEquals(CE.unsafeRunSync(attempt.use(Caio.pure)), Left(exception))
  }

  testGlobalAsync("combineK - should behave like orElse when underlying effect does") { params =>
    import params._

    forAll { (r1: Resource[CaioT, Int], r2: Resource[CaioT, Int]) =>
      val lhs = CE.unsafeRunSync(r1.orElse(r2).use(Caio.pure).attempt)
      val rhs = CE.unsafeRunSync((r1 <+> r2).use(Caio.pure).attempt)

      lhs <-> rhs
    }
  }

  testGlobalAsync("combineK - should behave like underlying effect") { params =>
    import params._
    import cats.data.OptionT
    forAll { (ot1: OptionT[CaioT, Int], ot2: OptionT[CaioT, Int]) =>
      val lhs: Either[Throwable, Option[Int]] =
        CE.unsafeRunSync(Resource.eval[OptionT[CaioT, *], Int](ot1 <+> ot2).use(OptionT.pure[CaioT](_)).value.attempt)
      val rhs: Either[Throwable, Option[Int]] =
        CE.unsafeRunSync(
          (Resource.eval[OptionT[CaioT, *], Int](ot1) <+> Resource.eval[OptionT[CaioT, *], Int](ot2))
            .use(OptionT.pure[CaioT](_))
            .value
            .attempt
        )

      lhs <-> rhs
    }
  }

  testAsync("parZip - releases resources in reverse order of acquisition") { params =>
    import params._

    forAll { (as: List[(Int, Either[Throwable, Unit])], rhs: Boolean) =>
      var released: List[Int] = Nil

      val r    = as.traverse { case (a, e) =>
        Resource.make[CaioT, Int](Caio(a))(a => Caio { released = a :: released } *> Caio.fromEither(e))
      }
      val unit = ().pure[Resource[CaioT, *]]
      val p    = if (rhs) r.both(unit) else unit.both(r)

      CE.unsafeToFuture(p.use(Caio.pure).attempt)
      EC.tick()
      released <-> as.map(_._1)
    }
  }

  testAsync("parZip - parallel acquisition and release") { params =>
    import params._

    var leftAllocated  = false
    var rightAllocated = false
    var leftReleasing  = false
    var rightReleasing = false
    var leftReleased   = false
    var rightReleased  = false

    val wait = Caio.sleep(200.millis)
    val lhs  = Resource.make[CaioT, Unit](wait *> Caio { leftAllocated = true }) { _ =>
      Caio { leftReleasing = true } *> wait *> Caio { leftReleased = true }
    }
    val rhs  = Resource.make[CaioT, Unit](wait *> Caio { rightAllocated = true }) { _ =>
      Caio { rightReleasing = true } *> wait *> Caio { rightReleased = true }
    }

    RealCE.unsafeRunSync((lhs, rhs).parTupled.use(_ => wait))

    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("parZip - safety: lhs error during rhs interruptible region") { params =>
    import params._

    var leftAllocated  = false
    var rightAllocated = false
    var leftReleasing  = false
    var rightReleasing = false
    var leftReleased   = false
    var rightReleased  = false

    def wait(n: Int): CaioT[Unit] = Caio.sleep(n.millis)
    val lhs                       = for {
      _ <- Resource.make(wait(1) *> Caio { leftAllocated = true }) { _ =>
             Caio { leftReleasing = true } *> wait(1) *> Caio { leftReleased = true }
           }
      _ <- Resource.eval[CaioT, Unit](wait(1) *> Caio.raiseError(new Exception))
    } yield ()

    val rhs = for {
      _ <- Resource.make(wait(100) *> Caio { rightAllocated = true }) { _ =>
             Caio { rightReleasing = true } *> wait(1) *> Caio { rightReleased = true }
           }
      _ <- Resource.eval(wait(200))
    } yield ()

    val caio =
      (lhs, rhs).parTupled
        .use(_ => Caio.unit)
        .handleError(_ => ())

    RealCE.unsafeRunSync(caio)

    assertEquals(leftReleasing, true)
    assertEquals(rightReleasing, true)
    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("parZip - safety: rhs error during lhs uninterruptible region") { params =>
    import params._

    var leftAllocated  = false
    var rightAllocated = false
    var rightErrored   = false
    var leftReleasing  = false
    var rightReleasing = false
    var leftReleased   = false
    var rightReleased  = false

    def wait(n: Int): CaioT[Unit] = Caio.sleep(n.millis)
    val lhs                       = Resource.make(wait(300) *> Caio { leftAllocated = true }) { _ =>
      Caio { leftReleasing = true } *> wait(100) *> Caio { leftReleased = true }
    }
    val rhs                       = for {
      _ <- Resource.make[CaioT, Unit](wait(100) *> Caio { rightAllocated = true }) { _ =>
             Caio { rightReleasing = true } *> wait(100) *> Caio { rightReleased = true }
           }
      _ <- Resource.make[CaioT, Unit](wait(100) *> Caio { rightErrored = true } *> Caio.raiseError(new Exception))(_ =>
             Caio.unit
           )
    } yield ()

    val caio =
      (lhs, rhs).parTupled
        .use(_ => wait(1))
        .handleError(_ => ())

    RealCE.unsafeRunSync(caio)

    assertEquals(leftReleasing, true)
    assertEquals(rightReleasing, true)
    assertEquals(leftAllocated, true)
    assertEquals(leftReleasing, true)
    assertEquals(leftReleased, true)
    assertEquals(rightReleased, true)
  }

  testAsync("onFinalizeCase - interruption") { params =>
    import params._

    def p: CaioT[ExitCase] =
      Deferred[CaioT, ExitCase]
        .flatMap { stop =>
          val r = Resource
            .eval(Caio.never: CaioT[Int])
            .onFinalizeCase(stop.complete(_).void)
            .use(Caio.pure(_))

          r.start.flatMap { fiber =>
            Caio.sleep(200.millis) *> fiber.cancel *> stop.get
          }
        }
        .timeout(2.seconds)

    val value = RealCE.unsafeRunSync(p)

    assertEquals(value, ExitCase.Canceled)
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

      CE.unsafeRunSync(caio)

      released <-> y :: acquired
    }
  }
}
