package caio.std

import caio.{Caio, FoldCaioSuccess, IOCaio, KleisliCaio}
import cats.Monoid
import cats.effect.{Async, IO}

class CaioAsync[C, V, L:Monoid] extends CaioSync[C, V, L] with Async[Caio[C, V, L, *]] {

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, V, L, A] =
    IOCaio(IO.async(k))

  /**
   * AsyncF application will discard any failure, Log or context change information.
   * @param k
   * @tparam A
   * @return
   */
  def asyncF[A](k: (Either[Throwable, A] => Unit) => Caio[C, V, L, Unit]): Caio[C, V, L, A] = {
    KleisliCaio[C, V, L, A]{ c =>
      val k2 = k.andThen { c0 => Caio.foldIO(c0, c).map(_ => ()) }
      IO.asyncF(k2).map(a => FoldCaioSuccess[C, V, L, A](c, Monoid[L].empty, a))
    }
  }

  /**
   * Important override otherwise can get caught in an infinite loop
   * @param ioa
   * @tparam A
   * @return
   */
  override def liftIO[A](ioa: IO[A]): Caio[C, V, L, A] =
    IOCaio(ioa)
}
