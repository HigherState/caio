//package caio.std
//
//import caio.Caio
//import caio.mtl.{ConcurrentEffectIsomorphism, Effectful}
//import cats.Monoid
//import cats.mtl.Ask
//
//class CaioEffectful[C, V, L: Monoid](ask: Ask[Caio[C, V, L, *], C], base: ConcurrentEffect[Caio[Unit, V, L, *]])
//    extends Effectful[Caio[C, V, L, *]] {
//  def concurrentEffect: Caio[C, V, L, ConcurrentEffect[Caio[C, V, L, *]]] =
//    ask.ask.map { (c: C) =>
//      val iso = new CaioBijectionK[C, Unit, V, L](_ => c, _ => ())
//      new ConcurrentEffectIsomorphism[Caio[C, V, L, *], Caio[Unit, V, L, *]](base, iso)
//    }
//}
