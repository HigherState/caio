package caio

import caio.std.CaioConcurrentEffect
import cats.effect.{ConcurrentEffect, ContextShift, IO, Resource}
import fs2.Chunk
import monix.eval.{Task, TaskLike}
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.{AsyncFunSpec, Matchers}

class StreamTests extends AsyncFunSpec with Matchers {

  import Event._
  type L = Vector[Event]
  type C = Unit
  type V = Failure


  type CaioT[A] = Caio[C, Failure, EventLog, A]

  it("Should stream Caio results via caio with resolution") {
    implicit val CS: ContextShift[IO]       = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val CE: ConcurrentEffect[CaioT] =
      new CaioConcurrentEffect[Unit, V, L](())((_, _) => IO.unit)((_, _, _) => IO.unit)((_, _, _) =>
        IO.unit
      )
    Resource
      .make(IO.delay("A STRING"))(_ => IO.unit)
      .use { _ =>
        Extraction.stream[CaioT]
      }
      .unsafeRunSync() shouldBe Vector[Byte](1, 2, 3, 4, 5)
  }
}

object Extraction {
  import fs2.interop.reactivestreams.PublisherOps
  implicit val CS: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

  def stream[M[_]](implicit CE: ConcurrentEffect[M]): IO[Vector[Byte]] =
    IO.suspend {
      val r: Resource[Task, Iterator[Byte]] =
        Resource
          .make[M, Iterator[Byte]](CE.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
            CE.delay(())
          }
          .mapK(implicitly[TaskLike[M]])
      CE.toIO {
        Observable
          .fromIterator(r)
          .map(Array(_))
          .map(Chunk.bytes)
          .toReactivePublisher[Chunk[Byte]](Scheduler.global)
          .toStream
          .flatMap(fs2.Stream.chunk)
          .compile
          .toVector
      }
    }

  def stream2[M[_]](implicit CE: ConcurrentEffect[M]): IO[Vector[Byte]] =
    IO.suspend {
      val r: Resource[Task, Iterator[Byte]] =
        Resource
          .make[Task, Iterator[Byte]](Task.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
            Task.delay(())
          }
      CE.toIO {
        Observable
          .fromIterator(r)
          .map(Array(_))
          .map(Chunk.bytes)
          .toReactivePublisher[Chunk[Byte]](Scheduler.global)
          .toStream[M]
          .flatMap(fs2.Stream.chunk)
          .compile
          .toVector
      }
    }

  //  def stream2[M[_]]() = {
  //    val r = Resource
  //      .make[M, Iterator[Byte]](CE.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
  //        CE.delay(())
  //      }
  //    fs2.Stream.resource(r).flatMap(a => fs2.Stream.fromIterator).compile.toVector
  //  }
}

