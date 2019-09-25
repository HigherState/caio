package caio.mtl

import caio._
import cats.effect.{IO, LiftIO}

trait CaioLiftIO extends LiftIO[Caio]{
  def liftIO[A](ioa: IO[A]): Caio[A] =
    CaioKleisli(_ => IOResult(ioa.map(SuccessResult(_, State.empty))))
}
