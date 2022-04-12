package caio.std

import caio.Caio
import caio.mtl.{DispatcherIsomorphism, Dispatch}
import cats.mtl.Ask
import cats.effect.Resource
import cats.effect.std.Dispatcher

class CaioDispatch[C, L](ask: Ask[Caio[C, L, *], C], base: Dispatcher[Caio[Unit, L, *]]) extends Dispatch[Caio[C, L, *]] {
  def dispatcher: Resource[Caio[C, L, *], Dispatcher[Caio[C, L, *]]] =
    Resource.eval(ask.ask).map { (c: C) =>
      val iso = new CaioBijectionK[C, Unit, L](_ => c, _ => ())
      new DispatcherIsomorphism[Caio[C, L, *], Caio[Unit, L, *]](base, iso)
    }
}