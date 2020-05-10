package caio.std

import caio.{Caio, IOCaio}
import cats.effect.{IO, LiftIO}

class CaioLiftIO[C, V, L] extends LiftIO[Caio[C, V, L, *]]{
  def liftIO[A](ioa: IO[A]): Caio[C, V, L, A] =
    IOCaio(ioa)
}
