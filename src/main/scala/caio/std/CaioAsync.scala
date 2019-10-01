package caio.std

import caio.{Caio, CaioKleisli, ResultOps}
import cats.Monoid
import cats.effect.{Async, IO}

class CaioAsync[C, V, L:Monoid] extends CaioSync[C, V, L] with Async[Caio[C, V, L, *]] {
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, V, L, A] =
    liftIO(IO.async(k))

  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
    CaioKleisli[C, V, L, A]{ c =>
      val k2 = k.andThen(_.toIOResult(c).io.map(_ => ()))
      ResultOps.fromIO(IO.asyncF(k2))
    }
}
