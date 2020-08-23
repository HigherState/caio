package caio.mtl

trait Expander[M[_], A] {

  def expand[B]:Expanded[M, A, B]
}


trait Expands[M2[_], M[_], A] {

  def apply[T](a: A, m:M2[T]):M[T]

  def lift[T](m:M[T]):M2[T]

}

//class Expanded[M[_], A, B](implicit sourceApplicativeAsk: ApplicativeAsk[M, A]) {
//  type M2[_]
//
//  implicit def applicativeAsk:ApplicativeAsk[M2, (A, B)]
//}



trait Expanded[M[_], A, B] {
  type M2[_]

  //implicit def monadState:MonadState[M2, (A, B)]

  implicit def expands:Expands[M2, M, B] with Expander[M2, (A, B)]

  def apply[T](b:B)(m:M2[T]):M[T]
}

//class BlahTemp {
//
//
//  def function[
//    M[_]:Monad:LiftIO,
//    M2[_]:Expands[M2, ?[_], String]]:Int = {
//
//  }
//
//  def grow[M[_]:Monad:LiftIO:Expander[?[_], Unit] = {
//
//    val expander = expands[M, Unit, String]
//  }
//}

object Expander {

  def expand[M[_], A, B](implicit E:Expander[M, A]):Expanded[M, A, B] =
    E.expand[B]
}

object Expands {

  def apply[M[_], M2[_], A, T](a: A, m:M2[T])(implicit E:Expands[M2, M, A]):M[T] =
    E.apply(a, m)

  def lift[M[_], M2[_], A, T](m:M[T])(implicit E:Expands[M2, M, A]):M2[T] =
    E.lift(m)
}
