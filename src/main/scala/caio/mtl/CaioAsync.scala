package caio.mtl

import caio._
import cats.effect.{Async, IO}

trait CaioAsync extends Async[Caio] with CaioSync {
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[A] =
    liftIO(IO.async(k))

  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[Unit]): Caio[A] =
    CaioKleisli[A]{ c =>
      val k2 = k.andThen(_.toIOResult(c).io.map(_ => ()))
      ResultOps.fromIO(IO.asyncF(k2))
    }
}
