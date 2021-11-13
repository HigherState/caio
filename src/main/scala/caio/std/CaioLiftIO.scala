package caio.std

import caio.Caio
import cats.effect.{IO, LiftIO}

trait CaioLiftIO[C, L] extends LiftIO[Caio[C, L, *]] {
  def liftIO[A](io: IO[A]): Caio[C, L, A] =
    Caio.liftIO(io)
}
