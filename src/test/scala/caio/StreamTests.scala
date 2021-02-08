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
        Extraction.stream3point5[CaioT]
      }
      .unsafeRunSync() shouldBe Vector[Byte](1, 2, 3, 4, 5)
  }

  it("Should stream IO results via IO with resolution") {
    implicit val CS: ContextShift[IO]       = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val CE: ConcurrentEffect[IO] =  IO.ioConcurrentEffect
    Resource
      .make(IO.delay("A STRING"))(_ => IO.unit)
      .use { _ =>
        Extraction.stream[IO]
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

  def stream3[M[_]](implicit CE: ConcurrentEffect[M]): IO[Vector[Byte]] =
    IO.suspend {
      val resource: Resource[M, Iterator[Byte]] = {
        Resource
          .make[M, Iterator[Byte]](CE.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
            CE.delay(())
          }
      }
      val allocated = resource.allocated
      val acquire = CE.map(allocated)(_._1)
      val release = CE.flatMap(allocated)(_._2)
      CE.toIO {
        Observable
          .resourceF(acquire)(_ => release)
          .flatMapIterable[Byte](_.toVector)
          .map(Array(_))
          .map(Chunk.bytes)
          .toReactivePublisher[Chunk[Byte]](Scheduler.global)
          .toStream
          .flatMap(fs2.Stream.chunk)
          .compile
          .toVector
      }
    }

  def stream3point5[M[_]](implicit CE: ConcurrentEffect[M]): IO[Vector[Byte]] =
    IO.suspend {
      val resource: Resource[M, Iterator[Byte]] = {
        Resource
          .make[M, Iterator[Byte]](CE.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
            CE.delay(())
          }
      }

      CE.toIO {
        Observable
          .resourceF(resource.allocated)(_._2)
          .map(_._1)
          .flatMapIterable[Byte](_.toVector)
          .map(Array(_))
          .map(Chunk.bytes)
          .toReactivePublisher[Chunk[Byte]](Scheduler.global)
          .toStream
          .flatMap(fs2.Stream.chunk)
          .compile
          .toVector
      }
    }

  def stream4[M[_]](implicit CE: ConcurrentEffect[M]): IO[Vector[Byte]] =
    IO.suspend {
      val r = Resource
        .liftF(CE.delay(Iterator[Byte](1, 2, 3, 4, 5)))
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


  //  def stream2[M[_]]() = {
  //    val r = Resource
  //      .make[M, Iterator[Byte]](CE.delay(Iterator(1, 2, 3, 4, 5))) { _ =>
  //        CE.delay(())
  //      }
  //    fs2.Stream.resource(r).flatMap(a => fs2.Stream.fromIterator).compile.toVector
  //  }
}

