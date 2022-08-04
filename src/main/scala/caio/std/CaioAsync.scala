package caio.std

import caio.Caio
import cats.Monoid
import cats.effect.{Async, IO}

class CaioAsync[C, L: Monoid] extends CaioSync[C, L] with Async[Caio[C, L, *]] {

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, L, A] =
    Caio.async[A](k)

  /**
   * AsyncF application will discard any failure, Log or context change information.
   * @param k
   * @tparam A
   * @return
   */
  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, L, Unit]): Caio[C, L, A] =
    Caio.asyncF[C, L, A](k)

  /**
   * Important override otherwise can get caught in an infinite loop
   * @param ioa
   * @tparam A
   * @return
   */
  override def liftIO[A](io: IO[A]): Caio[C, L, A] =
    Caio.liftIO(io)
}
