package caio.std

import caio.Caio
import caio.mtl.{Dispatch, DispatcherIsomorphism}
import cats.mtl.Ask
import cats.effect.Resource
import cats.effect.std.Dispatcher

class CaioDispatch[C, L](ask: Ask[Caio[C, L, _], C], base: Dispatcher[Caio[Unit, L, _]]) extends Dispatch[Caio[C, L, _]] {
  def dispatcher: Resource[Caio[C, L, _], Dispatcher[Caio[C, L, _]]] =
    Resource.eval(ask.ask).map { (c: C) =>
      val iso = new CaioBijectionK[C, Unit, L](_ => c, _ => ())
      new DispatcherIsomorphism[Caio[C, L, _], Caio[Unit, L, _]](base, iso)
    }
}
