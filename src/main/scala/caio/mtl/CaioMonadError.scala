package caio.mtl

import caio.Caio
import cats.MonadError

trait CaioMonadError extends MonadError[Caio, Throwable] with CaioApplicativeError with CaioMonad
