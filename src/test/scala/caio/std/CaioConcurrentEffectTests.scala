package caio.std

import caio.Event._
import caio.implicits.StaticImplicits
import caio.{Caio, Failure}
import cats.Monoid
import cats.effect.concurrent.Ref
import cats.effect.{Clock, ContextShift, IO, Timer}
import org.scalatest.{AsyncFunSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class CaioConcurrentEffectTests  extends AsyncFunSpec with Matchers{
//  import cats.implicits._
  type CaioT[A] = Caio[Unit, Failure, EventLog, A]

  implicit val CS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val C = new StaticImplicits[Unit, Failure, EventLog] {
    protected implicit def ML: Monoid[EventLog] = EventMonoid
  }
  import C._

  val effect = new CaioConcurrentEffect[Unit, Failure, EventLog](())((_, _) => IO.unit)((_, _, _) => IO.unit)((_, _, _) => IO.unit)

  describe("Async shouldnt loop") {

    it("Works with timer async case") {
      val T: Timer[IO] = IO.timer(ExecutionContext.global)

      val t = new Timer[CaioT] {
        val clock: Clock[CaioT]                          = Clock.create[CaioT]
        def sleep(duration: FiniteDuration): CaioT[Unit] =
          staticCaioConcurrent.liftIO(T.sleep(duration))
      }
      val b = effect.toIO(t.sleep(1.millis))
      b.unsafeRunSync() shouldBe ()

    }
  }

}
