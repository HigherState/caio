package caio.std

import caio.{Caio, IOCaio, KleisliCaio}
import cats.effect.kernel.Cont
import cats.effect.{Async, Deferred, IO}

import scala.concurrent.ExecutionContext

trait CaioAsync[C, L] extends CaioSync[C,  L] with CaioTemporal[C, L] with Async[Caio[C, L, *]] {


//  def start[A](fa: Caio[C, V, L, A]): Caio[C, V, L, Fiber[Caio[C, V, L, *], Throwable, A]] =
//    KleisliCaio[C, V, L, Fiber[Caio[C, V, L, *], A]] { c =>
//      val fiberIO: IO[Fiber[IO, FoldCaioPure[C, V, L, A]]] =
//        Caio.foldIO(fa, c).start
//      fiberIO.map { fiber =>
//        val cancel: IO[Unit] = fiber.cancel
//
//        val join = fiber.join
//        FoldCaioSuccess(c, Monoid[L].empty, Fiber(KleisliCaio[C, V, L, A](_ => join), IOCaio(cancel)))
//      }
//    }
//
//  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, V, L, A] =
//    Caio.async[A](k)
//
//  /**
//   * AsyncF application will discard any failure, Log or context change information.
//   * @param k
//   * @tparam A
//   * @return
//   */
//  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
//    Caio.asyncF[C, V, L, A](k)
//
//  /**
//   * Important override otherwise can get caught in an infinite loop
//   * @param ioa
//   * @tparam A
//   * @return
//   */
//  def liftIO[A](io: IO[A]): Caio[C, V, L, A] =
//    Caio.liftIO(io)

  def evalOn[A](fa: Caio[C, L, A], ec: ExecutionContext): Caio[C, L, A] =
    KleisliCaio[C, L, A] { c =>
      IO.asyncForIO.evalOn(Caio.foldIO(fa, c), ec)
    }

  def executionContext: Caio[C, L, ExecutionContext] =
    IOCaio(IO.asyncForIO.executionContext)

  def cont[K, R](body: Cont[Caio[C, L, *], K, R]): Caio[C, L, R] = ???

  def deferred[A]: Caio[C, L, Deferred[Caio[C, L, *], A]] =
    IOCaio(IO(Deferred.unsafe[Caio[C, L, *], A](this)))
}
