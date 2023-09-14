package caio.std

import caio.Caio
import caio.mtl.{DispatcherIsomorphism, Effectful}
import cats.effect.std.Dispatcher
import cats.mtl.Ask

class CaioEffectful[C, L](ask: Ask[Caio[C, L, _], C], base: Dispatcher[Caio[Unit, L, _]])
  extends Effectful[Caio[C, L, _]] {
  def dispatcher: Caio[C, L, Dispatcher[Caio[C, L, _]]] =
    ask.ask.map { (c: C) =>
      val iso = new CaioBijectionK[C, Unit, L](_ => c, _ => ())
      new DispatcherIsomorphism[Caio[C, L, _], Caio[Unit, L, _]](base, iso)
    }
}