package caio.std

import caio.Caio
import cats.effect.{IO, LiftIO}

class CaioLiftIO[C, V, L] extends LiftIO[Caio[C, V, L, *]]{
  def liftIO[A](io: IO[A]): Caio[C, V, L, A] =
    Caio.liftIO(io)
}