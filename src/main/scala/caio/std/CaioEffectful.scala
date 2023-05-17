package caio.std

import caio.Caio
import caio.mtl.{DispatcherIsomorphism, Effectful}
import cats.effect.std.Dispatcher
import cats.mtl.Ask

class CaioEffectful[C, L](ask: Ask[Caio[C, L, *], C], base: Dispatcher[Caio[Unit, L, *]])
  extends Effectful[Caio[C, L, *]] {
  def dispatcher: Caio[C, L, Dispatcher[Caio[C, L, *]]] =
    ask.ask.map { (c: C) =>
      val iso = new CaioBijectionK[C, Unit, L](_ => c, _ => ())
      new DispatcherIsomorphism[Caio[C, L, *], Caio[Unit, L, *]](base, iso)
    }
}