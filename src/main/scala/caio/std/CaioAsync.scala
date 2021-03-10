package caio.std

import caio.Caio
import cats.Monoid
import cats.effect.{Async, IO}

class CaioAsync[C, V, L: Monoid] extends CaioSync[C, V, L] with Async[Caio[C, V, L, *]] {

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, V, L, A] =
    Caio.async[A](k)

  /**
   * AsyncF application will discard any failure, Log or context change information.
   * @param k
   * @tparam A
   * @return
   */
  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] =
    Caio.asyncF[C, V, L, A](k)

  /**
   * Important override otherwise can get caught in an infinite loop
   * @param ioa
   * @tparam A
   * @return
   */
  override def liftIO[A](io: IO[A]): Caio[C, V, L, A] =
    Caio.liftIO(io)
}
