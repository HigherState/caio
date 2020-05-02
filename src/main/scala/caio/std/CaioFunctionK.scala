package caio.std

import caio.{Caio, CaioKleisli, ErrorResult, IOResult, LogStore, SuccessResult}
import cats.Monoid
import cats.arrow.FunctionK

class CaioFunctionK[C1, C2, V, L: Monoid](f: C2 => C1)
  extends FunctionK[Caio[C1, V, L, *], Caio[C2, V, L, *]] {
  def apply[A](fa: Caio[C1, V, L, A]): Caio[C2, V, L, A] =
    CaioKleisli[C2, V, L, A] { c2 =>
      IOResult {
        fa.eval(f(c2)).map {
          case (Left(e), _, l) =>
            ErrorResult(e, LogStore(l, implicitly[Monoid[L]]))
          case (Right(a), _, l) =>
            SuccessResult(a, LogStore(l, implicitly[Monoid[L]]))
        }

      }
    }
}
