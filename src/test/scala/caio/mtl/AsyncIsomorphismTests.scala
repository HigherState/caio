package caio.mtl

import caio.BijectionK
import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.effect.{Async, IO}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class AsyncIsomorphismTests extends AnyFunSuite with Matchers {
  private type F[A] = Kleisli[IO, Unit, A]

  private val isomorphism: BijectionK[F, IO] = new BijectionK[F, IO] {
    def apply[A](fa: F[A]): IO[A] =
      fa.run(())

    def unapply[A](fa: IO[A]): F[A] =
      Kleisli.liftF(fa)

    def invert: FunctionK[IO, F] = new FunctionK[IO, F] {
      def apply[A](fa: IO[A]): F[A] =
        unapply(fa)
    }
  }

  implicit private val async: Async[F] = new AsyncIsomorphism[F, IO](Async[IO], isomorphism)

  test("ref preserves reads, writes and the result of an atomic modification") {
    val program = for {
      ref      <- async.ref(10)
      initial  <- ref.get
      _        <- ref.set(20)
      previous <- ref.modify(value => (value + 5, value))
      current  <- ref.get
    } yield (initial, previous, current)

    program.run(()).timeout(5.seconds).unsafeRunSync() shouldBe ((10, 20, 25))
  }

  test("ref access rejects stale and reused setters without overwriting newer values") {
    val program = for {
      ref          <- async.ref(0)
      stale        <- ref.access
      _            <- ref.set(1)
      staleUpdated <- stale._2(2)
      afterStale   <- ref.get
      fresh        <- ref.access
      freshUpdated <- fresh._2(3)
      reused       <- fresh._2(4)
      current      <- ref.get
    } yield (stale._1, staleUpdated, afterStale, fresh._1, freshUpdated, reused, current)

    program.run(()).timeout(5.seconds).unsafeRunSync() shouldBe ((0, false, 1, 1, true, false, 3))
  }

  test("ref updates remain atomic across concurrent effects") {
    val program = for {
      ref     <- async.ref(0)
      _       <- (0 until 1000).toList.parTraverse_(_ => ref.update(_ + 1))
      current <- ref.get
    } yield current

    program.run(()).timeout(5.seconds).unsafeRunSync() shouldBe 1000
  }
}
