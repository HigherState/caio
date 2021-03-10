package caio

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import caio.Event.EventLog
//import caio.implicits.StaticImplicits
import caio.std.{CaioConcurrentEffect, CaioMonad}
import caio.std.Par.unwrap
import cats.Monoid
import cats.effect.{ContextShift, IO}
import cats.effect.laws.util.TestContext
import cats.kernel.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms.invariant
import munit.{DisciplineSuite, Location}
import org.scalacheck.Prop
import org.typelevel.discipline.Laws

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

class TestInstances extends DisciplineSuite with cats.effect.laws.util.TestInstances  {
  type C = Map[String, String]
  type L = EventLog
  type V = Failure
  type CaioT[A] = Caio[C, V, L, A]

  //val implicits: StaticImplicits[C, V, L] = new StaticImplicits[C, V, L]
  //import implicits._

  implicit val C = Map.empty[String, String]
  implicit val isomorphism = invariant[CaioT](new CaioMonad[C, V, L])

  protected class TestParams(testContext: TestContext) {
    implicit val EC: TestContext = testContext
    implicit val CS: ContextShift[IO] = testContext.ioContextShift
    implicit val CE: CaioConcurrentEffect[C, V, L] = concurrentEffect(CS)
  }

  //def concurrentEffect(context: TestContext): CaioConcurrentEffect[C, V, L] =
  //  concurrentEffect(context.ioContextShift)

  def concurrentEffect(cs: ContextShift[IO]): CaioConcurrentEffect[C, V, L] =
    new CaioConcurrentEffect[C, V, L](C)((_, _) => IO.unit)((_, _, _) => IO.unit)((_, _, _) => IO.unit)(Event.EventMonoid, cs)

  def checkAllAsync(name: String)(f: TestParams => Laws#RuleSet): Unit = {
    val context = TestContext()
    val ruleSet = f(new TestParams(context))

    for ((id, prop) <- ruleSet.all.properties)
      property(name + "." + id)(Prop(params => silenceSystemErr(() => prop(params))))(Location.empty)
  }

  def testAsync(name: String)(f: CaioConcurrentEffect[C, V, L] => Unit): Unit = {
    val CS = IO.contextShift(ExecutionContext.global)
    val CE = concurrentEffect(CS)
    property(name)(silenceSystemErr(() => f(CE)))(Location.empty)
  }

  private def silenceSystemErr[A](thunk: () => A): A = synchronized {
    val oldErr = System.err
    val outStream = new ByteArrayOutputStream()
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)

    try {
      thunk()
    } catch {
      case NonFatal(e) =>
        // In case of errors, print whatever was caught
        fakeErr.close()
        val out = new String(outStream.toByteArray, StandardCharsets.UTF_8)
        if (out.nonEmpty) oldErr.println(out)
        throw e
    } finally {
      fakeErr.close()
      System.setErr(oldErr)
    }
  }

  implicit def eqCaio[C, V, L: Monoid, A: Eq](implicit C: C, ec: TestContext): Eq[Caio[C, V, L, A]] =
    (x: Caio[C, V, L, A], y: Caio[C, V, L, A]) =>
      eqIO[A].eqv(x.run(C), y.run(C))

  implicit def eqCaioPar[C, V, L: Monoid, A: Eq](implicit C: C, ec: TestContext): Eq[ParCaio[C, V, L, A]] =
    (x: ParCaio[C, V, L, A], y: ParCaio[C, V, L, A]) =>
      eqCaio[C, V, L, A].eqv(unwrap(x), unwrap(y))
}