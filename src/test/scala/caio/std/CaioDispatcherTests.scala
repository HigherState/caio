package caio.std

import caio.Caio
import cats.effect.{Deferred, IO, Resource}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

class CaioDispatcherTests extends AnyFunSuite with Matchers {
  test("unsafeRunSync cancels the effect and releases its resource when the caller is interrupted") {
    val dispatcher = Resource.make(CaioDispatcher.unsafeIO[Unit, Unit](())()()) { dispatcher =>
      dispatcher.toIO(dispatcher.unsafeClose).void
    }

    dispatcher
      .use { dispatcher =>
        IO.blocking {
          val started   = new CountDownLatch(1)
          val cancelled = new CountDownLatch(1)
          val released  = new CountDownLatch(1)
          val failure   = new AtomicReference[Option[Throwable]](None)
          val finish    = Deferred[IO, Unit].unsafeRunSync()
          val effect    = Resource.make(IO.unit)(_ => IO(released.countDown())).use { _ =>
            (IO(started.countDown()) *> finish.get).onCancel(IO(cancelled.countDown()))
          }
          val caller    = new Thread(() =>
            try dispatcher.unsafeRunSync(Caio.liftIO(effect))
            catch { case error: Exception => failure.set(Some(error)) }
          )

          caller.setDaemon(true)
          caller.start()
          try {
            withClue("Effect did not start: ") {
              started.await(5, TimeUnit.SECONDS) shouldBe true
            }
            caller.interrupt()
            caller.join(5000)

            caller.isAlive shouldBe false
            failure.get().exists(_.isInstanceOf[InterruptedException]) shouldBe true
            withClue("Caller interruption did not release the resource: ") {
              released.await(3, TimeUnit.SECONDS) shouldBe true
            }
            withClue("Effect did not observe cancellation: ") {
              cancelled.getCount shouldBe 0L
            }
          } finally {
            finish.complete(()).unsafeRunSync()
            caller.interrupt()
            caller.join(5000)
            val _ = withClue("Probe cleanup did not release the resource: ") {
              released.await(5, TimeUnit.SECONDS) shouldBe true
            }
          }
        }
      }
      .unsafeRunSync()
  }
}
