package caio.std

import caio.{Caio, CaioKleisli, ErrorResult, IOResult, LogStore, SuccessResult, mtl}
import caio.mtl._
import cats.Monoid
import cats.arrow.FunctionK
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=


case class CaioExtender[C, V, L:Monoid, E1](
  applicativeAsk:ApplicativeAsk[Caio[C, V, L, *], E1],
  monadState:MonadState[Caio[C, V, L, *], E1],
  )(implicit m:Mixer[C, E1], invert:Mixer[(E1, Unit), C]) extends Extender[Caio[C, V, L, *], E1] {

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[C, V, L, *], E1, E2] =
    CaioExtends[C, V, L, E1, E2](
      new CaioApplicativeAsk[(E1, E2), V, L],
      new CaioMonadState[(E1, E2), V, L]
    )

}

case class CaioExtends[C, V, L:Monoid, E1, E2](
  applicativeAsk:ApplicativeAsk[Caio[(E1, E2), V, L, *], (E1, E2)],
  monadState:MonadState[Caio[(E1, E2), V, L, *], (E1, E2)]
 )(implicit mixer:Mixer[C, E1], invert:Mixer[(E1, Unit), C]) extends Extends[Caio[C, V, L, *], E1, E2] {

  type FE[A] = Caio[(E1, E2), V, L, A]

  def extender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]): Extender[FE, E3] =
    CaioExtender[(E1, E2), V, L, E3](
      new MixedApplicativeAsk[FE, (E1, E2), E3](applicativeAsk),
      new MixedMonadState[FE, (E1, E2), E3](monadState)
    )



  def apply[A](e2:E2)(f:Caio[(E1, E2), V, L, A]):Caio[C, V, L, A] =
    CaioKleisli[C, V, L, A]{ c =>
      IOResult{
        f.eval(mixer.mix(c) -> e2).map {
          case (Left(e), _, l) =>
            ErrorResult(e, LogStore(l))
          case (Right(a), _, l) =>
            SuccessResult(a, LogStore(l))
        }
      }
    }

  def functionK: FunctionK[Caio[C, V, L, *], FE] =
    new CaioFunctionK[C, (E1, E2), V, L](p => invert.mix(p._1 -> ()))

}


object CaioProvider {
  implicit val M:Mixer[Unit, Unit] = new Mixer[Unit, Unit] {
    override def mix(a: Unit): Unit = ()

    override def inject(b: Unit, a: Unit): Unit = ()
  }
  implicit def unit[V, L:Monoid]:Provider[Caio[Unit, V, L, *]] =
    CaioExtender[Unit, V, L, Unit](
      new CaioApplicativeAsk[Unit, V, L],
      new CaioMonadState[Unit, V, L]
    )
}