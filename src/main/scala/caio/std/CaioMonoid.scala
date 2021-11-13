package caio.std

import cats.Monoid

trait CaioMonoid[L] {
 implicit def monoid:Monoid[L]
}
