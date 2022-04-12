package caio

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets

import caio.Event.EventLog
import caio.implicits.DynamicContextImplicits
import caio.std.CaioDispatcher

import cats.{Eq, SemigroupK, Order}
import cats.laws.IsEq
import cats.laws.discipline.SemigroupalTests.Isomorphisms.invariant
import cats.effect.{Async, IO, Outcome}
import cats.effect.std.Dispatcher
import cats.effect.testkit.{TestContext, TestInstances => CatsTestInstances}
import cats.effect.unsafe.{Scheduler, IORuntime, IORuntimeConfig}
import cats.effect.unsafe.implicits.global

import munit.{DisciplineSuite, Location}
import org.scalacheck.{Cogen, Prop}
import org.scalacheck.util.Pretty
import org.typelevel.discipline.Laws

import scala.util.control.NonFatal
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration.{FiniteDuration, Duration}

class TestInstances extends DisciplineSuite with CatsTestInstances {
  type C = Map[String, String]
  type L = EventLog
  type CaioT[A] = Caio[C, L, A]

  val implicits: DynamicContextImplicits[L] = new DynamicContextImplicits[L]
  implicit val isomorphism = invariant[CaioT](implicits.dynamicCaioMonad[C])

  implicit val C = Map.empty[String, String]

  protected class TestParams {
    implicit val T: Ticker = Ticker()
    implicit val EC: TestContext = T.ctx
    implicit val CA: Async[CaioT] = implicits.dynamicCaioAsync[C]
    implicit val RealCE = CaioDispatcher.unsafe[C, L](C)((_, _) => IO.unit)((_, _, _) => IO.unit)

    val blocking = IORuntime.createDefaultBlockingExecutionContext("blocking")._1
    val scheduler = Scheduler.createDefaultScheduler()._1
    val runtimeConfig = IORuntimeConfig()
    implicit val runtime = IORuntime.apply(EC, blocking, scheduler, () => (), runtimeConfig)
    
    implicit val CE = new Dispatcher[Caio[C, L, *]] {
      def unsafeToFutureCancelable[A](fa: Caio[C, L, A]): (Future[A], () => Future[Unit]) =
        (fa.run(C).unsafeToFuture()(runtime), () => Future.successful(()) )

      override def unsafeRunSync[A](fa: Caio[C, L, A]): A = {
        val (future, _) = unsafeToFutureCancelable(fa)

        EC.tick()

        try Await.result(future, Duration.Inf)
        catch {
          case t: TimeoutException =>
            throw t
        }
      }
    }

    implicit val SGK: SemigroupK[CaioT] = new SemigroupK[CaioT] {
      override def combineK[A](x: CaioT[A], y: CaioT[A]): CaioT[A] = x <* y
    }
  }

  def checkAllAsync(name: String)(f: TestParams => Laws#RuleSet): Unit = {
    val testParams = new TestParams
    val ruleSet = f(testParams)
    val realCE = testParams.RealCE

    for ((id, prop) <- ruleSet.all.properties)
      property(name + "." + id)(Prop(params => silenceSystemErr(() => prop(params))))(Location.empty)

    realCE.toIO(realCE.unsafeClose).unsafeRunSync()
    ()
  }

  def testAsync(name: String)(f: TestParams => Any): Unit = {
    val testParams = new TestParams
    val realCE = testParams.RealCE

    property(name){
      silenceSystemErr(() => { 
        f(testParams)
        try { realCE.toIO(realCE.unsafeClose).unsafeRunSync() } catch { case _: Exception => }
        () 
      })
    }(Location.empty)
  }

  def testOneAsync(name: String)(f: TestParams => Prop): Unit = {
    val testParams = new TestParams
    val realCE = testParams.RealCE
    property(name)(Prop(params => silenceSystemErr(() => { f(testParams)(params) })))(Location.empty)
    realCE.toIO(realCE.unsafeClose).unsafeRunSync()
    ()
  }

  def testGlobalAsync(name: String)(f: TestParams => Any): Unit = {
    property(name)(silenceSystemErr(() => { f(new TestParams); () }))(Location.empty)
  }

  protected def silenceSystemErr[A](thunk: () => A): A = synchronized {
    val oldErr = System.err
    val outStream = new ByteArrayOutputStream()
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)

    try {
      thunk()
    } catch {
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
    val oldErr = System.err
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)
    try {
      thunk
    } finally {
      System.setErr(oldErr)
      fakeErr.close()
    }
  }

  implicit def eqCaio[C0, L0, A: Eq](implicit C: C0, T: Ticker): Eq[Caio[C0, L0, A]] =
    Eq.by(caio => unsafeRun(caio.run(C)))

  implicit def caioIsEqToProp[C0, L0, A: Eq](implicit C: C0, T: Ticker, pp: Caio[C0, L0, A] => Pretty): IsEq[Caio[C0, L0, A]] => Prop =
    isEq =>
      if (eqCaio[C0, L0, A].eqv(isEq.lhs, isEq.rhs)) Prop.proved
      else
        Prop.falsified :| {
          val exp = Pretty.pretty[Caio[C0, L0, A]](isEq.lhs, Pretty.Params(0))
          val act = Pretty.pretty[Caio[C0, L0, A]](isEq.rhs, Pretty.Params(0))
          s"Expected: $exp\n" + s"Received: $act"
        }

  implicit def orderCaioFiniteDuration(implicit T: Ticker): Order[CaioT[FiniteDuration]] =
    Order by { caio => unsafeRun(caio.run(C)).fold(None, _ => None, fa => fa) }

  implicit def caioBooleanToProp(caio: CaioT[Boolean])(implicit T: Ticker): Prop =
    Prop(unsafeRun(caio.run(C)).fold(false, _ => false, _.getOrElse(false)))

  implicit def cogenCaio[C0, L0, A: Cogen](implicit C: C0, T: Ticker): Cogen[Caio[C0, L0, A]] =
    Cogen[Outcome[Option, Throwable, A]].contramap(caio => unsafeRun(caio.run(C)))
}