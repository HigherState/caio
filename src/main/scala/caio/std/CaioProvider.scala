package caio.std

import caio.{Caio, CaioKleisli, ErrorResult, IOResult, LogStore, SuccessResult, mtl}
import caio.mtl._
import cats.Monoid
import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

class CaioProvider[V, L:Monoid] extends Provider[Caio[Unit, V, L, *]] {

  def apply[E]: Extends[Caio[Unit, V, L, *], Unit, E] =
    new CaioProvides[V, L, E]
}

case class CaioExtender[V, L:Monoid, E1](
    applicativeAsk:ApplicativeAsk[Caio[E1, V, L, *], E1],
    monadState:MonadState[Caio[E1, V, L, *], E1]
  ) extends Extender[Caio[E1, V, L, *], E1] {

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[E1, V, L, *], E1, E2] =
    CaioExtends[V, L, E1, E2](
      new CaioApplicativeAsk[(E1, E2), V, L],
      new CaioMonadState[(E1, E2), V, L]
    )

}

case class CaioExtends[V, L:Monoid, E1, E2](
  applicativeAsk:ApplicativeAsk[Caio[(E1, E2), V, L, *], (E1, E2)],
  monadState:MonadState[Caio[(E1, E2), V, L, *], (E1, E2)]
 ) extends Extends[Caio[E1, V, L, *], E1, E2] {

  type FE[A] = Caio[(E1, E2), V, L, A]


  def extender[E3](mixer: Mixer[(E1, E2), E3]): Extender[FE, E3] =
    CaioExtender[V, L, E3](
      new MixedApplicativeAsk[](applicativeAsk, mixer),
      new MixedMonadState(monadState, mixer)
    )

  def apply[A](e2:E2)(f:Caio[(E1, E2), V, L, A]):Caio[E1, V, L, A] =
    CaioKleisli[E1, V, L, A]{ e1 =>
      IOResult{
        f.eval(e1 -> e2).map {
          case (Left(e), _, l) =>
            ErrorResult(e, LogStore(l))
          case (Right(a), _, l) =>
            SuccessResult(a, LogStore(l))
        }
      }
    }

  def functionK: FunctionK[Caio[E1, V, L, *], FE] =
    new FunctionK[Caio[E1, V, L, *], FE] {
      def apply[A](f: Caio[E1, V, L, A]): Caio[(E1, E2), V, L, A] =
        CaioKleisli[(E1, E2), V, L, A] { case (e1, _) =>
          IOResult {
            f.eval(e1).map {
              case (Left(e), _, l) =>
                ErrorResult(e, LogStore(l))
              case (Right(a), _, l) =>
                SuccessResult(a, LogStore(l))
            }

          }
        }
    }

}


object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}