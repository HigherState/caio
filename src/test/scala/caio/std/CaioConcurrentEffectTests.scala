package caio.std

import caio.Event._
import caio.implicits.StaticImplicits
import caio.{Caio, Failure}
import cats.effect.{Clock, ContextShift, IO, Timer}
import org.scalatest.{AsyncFunSpec, Matchers}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class CaioConcurrentEffectTests  extends AsyncFunSpec with Matchers{
//  import cats.implicits._
  type CaioT[A] = Caio[Unit, Failure, EventLog, A]

  implicit val CS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val C = new StaticImplicits[Unit, Failure, EventLog]()
  import C._

  describe("Async shouldnt loop") {

    it("Works with timer async case") {
      val T: Timer[IO] = IO.timer(ExecutionContext.global)

      val t = new Timer[CaioT] {
        val clock: Clock[CaioT]                          = Clock.create[CaioT]
        def sleep(duration: FiniteDuration): CaioT[Unit] =
          staticCaioConcurrent.liftIO(T.sleep(duration))
      }
      val b = t.sleep(1.millis).eval(())
      b.unsafeRunSync()._1 shouldBe Right(())

    }
    it("Blah") {

      val bob = IO.pure("a").flatMap(a => throw new Exception("a"))
      bob.attempt.unsafeRunSync()
    }
  }

}
