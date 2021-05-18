package caio

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import caio.Event.EventLog
import caio.implicits.DynamicContextImplicits
import caio.std.CaioConcurrentEffect
import caio.std.Par.unwrap
import cats.{Monoid, SemigroupK}
import cats.effect.{Clock, ContextShift, IO, Timer}
import cats.effect.laws.util.TestContext
import cats.kernel.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms.invariant
import cats.effect.laws.util.{TestInstances => CatsTestInstances}
import cats.laws.IsEq
import munit.{DisciplineSuite, Location}
import org.scalacheck.Prop
import org.scalacheck.util.Pretty
import org.typelevel.discipline.Laws

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

class TestInstances extends DisciplineSuite with CatsTestInstances {
  type C        = Map[String, String]
  type L        = EventLog
  type V        = Failure
  type CaioT[A] = Caio[C, V, L, A]

  val implicits: DynamicContextImplicits[V, L] = new DynamicContextImplicits[V, L]

  implicit val C           = Map.empty[String, String]
  implicit val isomorphism = invariant[CaioT](implicits.dynamicCaioMonad[C])

  protected class TestParams(testContext: TestContext, contextShift: ContextShift[IO]) {
    def this(testContext: TestContext) = this(testContext, testContext.ioContextShift)
    implicit val EC: TestContext                   = testContext
    implicit val CS: ContextShift[IO]              = contextShift
    implicit val CE: CaioConcurrentEffect[C, V, L] = concurrentEffect(CS)
    implicit val SGK: SemigroupK[CaioT]            = new SemigroupK[CaioT] {
      override def combineK[A](x: CaioT[A], y: CaioT[A]): CaioT[A] = x <* y
    }
  }

  def concurrentEffect(cs: ContextShift[IO]): CaioConcurrentEffect[C, V, L] =
    new CaioConcurrentEffect[C, V, L](C)((_, _) => IO.unit)((_, _, _) => IO.unit)((_, _, _) => IO.unit)(
      Event.EventMonoid,
      cs
    )

  def checkAllAsync(name: String)(f: TestParams => Laws#RuleSet): Unit = {
    val context = TestContext()
    val ruleSet = f(new TestParams(context))

    for ((id, prop) <- ruleSet.all.properties)
      property(name + "." + id)(Prop(params => silenceSystemErr(() => prop(params))))(Location.empty)
  }

  def testAsync(name: String)(f: TestParams => Any): Unit = {
    val context = TestContext()
    property(name)(silenceSystemErr { () => f(new TestParams(context)); () })(Location.empty)
  }

  def testGlobalAsync(name: String)(f: TestParams => Any): Unit = {
    val context = TestContext()
    val CS      = IO.contextShift(ExecutionContext.global)
    property(name)(silenceSystemErr { () => f(new TestParams(context, CS)); () })(Location.empty)
  }

  def getTimer(ioTimer: Timer[IO])(implicit CS: ContextShift[IO], CE: CaioConcurrentEffect[C, V, L]): Timer[CaioT] =
    new Timer[CaioT] {
      val clock: Clock[CaioT]                          = Clock.create[CaioT]
      def sleep(duration: FiniteDuration): CaioT[Unit] =
        implicits.dynamicCaioConcurrent[C].liftIO(ioTimer.sleep(duration))
    }

  protected def silenceSystemErr[A](thunk: () => A): A = synchronized {
    val oldErr    = System.err
    val outStream = new ByteArrayOutputStream()
    val fakeErr   = new PrintStream(outStream)
    System.setErr(fakeErr)

    try thunk()
    catch {
      case NonFatal(e) =>
        fakeErr.close()
        val out = new String(outStream.toByteArray, StandardCharsets.UTF_8)
        if (out.nonEmpty) oldErr.println(out)
        throw e
    } finally {
      fakeErr.close()
      System.setErr(oldErr)
    }
  }

  protected def catchSystemErr(thunk: => Unit): String = {
    val outStream = new ByteArrayOutputStream()
    catchSystemErrInto(outStream)(thunk)
    new String(outStream.toByteArray, StandardCharsets.UTF_8)
  }

  protected def catchSystemErrInto[T](outStream: OutputStream)(thunk: => T): T = synchronized {
    val oldErr  = System.err
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)
    try thunk
    finally {
      System.setErr(oldErr)
      fakeErr.close()
    }
  }

  implicit def eqCaio[C, V, L: Monoid, A: Eq](implicit C: C, ec: TestContext): Eq[Caio[C, V, L, A]] =
    (x: Caio[C, V, L, A], y: Caio[C, V, L, A]) => eqIO[A].eqv(x.run(C), y.run(C))

  implicit def eqCaioPar[C, V, L: Monoid, A: Eq](implicit C: C, ec: TestContext): Eq[ParCaio[C, V, L, A]] =
    (x: ParCaio[C, V, L, A], y: ParCaio[C, V, L, A]) => eqCaio[C, V, L, A].eqv(unwrap(x), unwrap(y))

  implicit def caioIsEqToProp[C, V, L: Monoid, A: Eq](implicit
    C: C,
    ec: TestContext,
    pp: Caio[C, V, L, A] => Pretty
  ): IsEq[Caio[C, V, L, A]] => Prop =
    isEq =>
      if (eqCaio[C, V, L, A].eqv(isEq.lhs, isEq.rhs)) Prop.proved
      else
        Prop.falsified :| {
          val exp = Pretty.pretty[Caio[C, V, L, A]](isEq.lhs, Pretty.Params(0))
          val act = Pretty.pretty[Caio[C, V, L, A]](isEq.rhs, Pretty.Params(0))
          s"Expected: $exp\n" + s"Received: $act"
        }
}
