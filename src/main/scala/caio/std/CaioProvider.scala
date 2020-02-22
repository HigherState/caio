package caio.std

import caio.{Caio, CaioKleisli, ErrorResult, IOResult, LogStore, SuccessResult, mtl}
import caio.mtl._
import cats.Monoid
import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=


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


  override def extender[E3](M: Mixer[(E1, E2), E3]): Extender[FE, E3] = ???

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
    new CaioFunctionK[E1, (E1, E2), V, L](_._1)

}


object CaioProvider {
  implicit def unit[V, L:Monoid]:Provider[Caio[Unit, V, L, *]] =
    CaioExtender[V, L, Unit](
      new CaioApplicativeAsk[Unit, V, L],
      new CaioMonadState[Unit, V, L]
    )
}