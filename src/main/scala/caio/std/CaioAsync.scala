package caio.std

import caio.{Caio, FoldCaioIO, FoldCaioSuccess, IOCaio, KleisliCaio}
import cats.{Eq, Monoid}
import cats.effect.{Async, IO}

class CaioAsync[C, V, L:Monoid:Eq] extends CaioSync[C, V, L] with Async[Caio[C, V, L, *]] {

  def async[A](k: (Either[Throwable, A] => Unit) => Unit): Caio[C, V, L, A] =
    //Don't use async.liftIO as this will create an infinite loop
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
      FoldCaioIO(IO.asyncF(k2).map(a => FoldCaioSuccess[C, V, L, A](c, Monoid[L].empty, a)))
    }
  }
}
