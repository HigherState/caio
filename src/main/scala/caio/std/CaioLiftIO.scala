package caio.std

import caio.{Caio, IOCaio}
import cats.Monoid
import cats.effect.{IO, LiftIO}

class CaioLiftIO[C, V, L:Monoid] extends LiftIO[Caio[C, V, L, *]]{
  def liftIO[A](ioa: IO[A]): Caio[C, V, L, A] =
    IOCaio(ioa)
}
