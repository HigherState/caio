package caio.mtl

import caio.Caio
import cats.effect.Bracket

trait CaioBracket extends Bracket[Caio, Throwable] {

}
