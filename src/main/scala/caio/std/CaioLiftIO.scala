package caio.std

import caio.Caio
import cats.effect.{IO, LiftIO}

trait CaioLiftIO[C, L] extends LiftIO[Caio[C, L, _]] {
  def liftIO[A](io: IO[A]): Caio[C, L, A] =
    Caio.liftIO(io)
}

object CaioLiftIO {
  def apply[C, L]: CaioLiftIO[C, L] =
    new CaioLiftIO[C, L] {}
}
