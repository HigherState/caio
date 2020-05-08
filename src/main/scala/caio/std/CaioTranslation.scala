//package caio.std
//
//import caio.Caio
//import cats.{Eq, Monoid, ~>}
//import io.typechecked.alphabetsoup.Mixer
//
//class CaioTranslation[E1, E2, V, L: Monoid:Eq, C](
//   implicit M: Mixer[(E1, C), E2],
//   I: Mixer[(E2, Unit), E1],
// ) {
//
//  def forwardsFunctionK(c:C): Caio[E1, V, L, *] ~> Caio[E2, V, L, *] =
//    new CaioContextFunctionK[E1, E2, V, L](e2 => I.mix(e2 -> ()),  e1 => M.mix(e1 -> c))
//
//  def backwardsFunctionK(c: C): Caio[E2, V, L, *] ~> Caio[E1, V, L, *] =
//    new CaioContextFunctionK[E2, E1, V, L](e1 => M.mix(e1 -> c), )
//
//}