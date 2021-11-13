package caio.std

import caio.{Caio, IOCaio}
import cats.effect.{IO, Unique}

trait CaioUnique[C, L] extends Unique[Caio[C, L, *]] {

  def unique: Caio[C, L, Unique.Token] =
    IOCaio(IO.delay(new Unique.Token))

}
