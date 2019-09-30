package caio.std

import caio.{Caio, CaioKleisli, ResultOps}
import cats.effect.{IO, LiftIO}

trait CaioLiftIO extends LiftIO[Caio]{
  def liftIO[A](ioa: IO[A]): Caio[A] =
    CaioKleisli(_ => ResultOps.fromIO(ioa))
}
