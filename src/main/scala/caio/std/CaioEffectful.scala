package caio.std

import caio.Caio
import caio.mtl.{ConcurrentEffectIsomorphism, Effectful}
import cats.Monoid
import cats.effect.ConcurrentEffect
import cats.mtl.Ask

class CaioEffectful[C, L: Monoid](ask: Ask[Caio[C, L, *], C], base: ConcurrentEffect[Caio[Unit, L, *]])
    extends Effectful[Caio[C, L, *]] {
  def concurrentEffect: Caio[C, L, ConcurrentEffect[Caio[C, L, *]]] =
    ask.ask.map { (c: C) =>
      val iso = new CaioBijectionK[C, Unit, L](_ => c, _ => ())
      new ConcurrentEffectIsomorphism[Caio[C, L, *], Caio[Unit, L, *]](base, iso)
    }
}
