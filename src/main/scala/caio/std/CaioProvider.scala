package caio.std

import caio.{Caio, CaioKleisli, ErrorResult, IOResult, LogStore, SuccessResult, mtl}
import caio.mtl._
import cats.Monoid
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

class CaioProvider[V, L:Monoid] extends Provider[Caio[Unit, V, L, *]] {
  def apply[E]: Provides[Caio[Unit, V, L, *], E] =
    new CaioProvides[V, L, E]
}

class CaioExtender[V, L:Monoid, E1] extends Extender[Caio[E1, V, L, *], E1] {

  def apply[E2](implicit M: Mixer[(E1, E2), E2], EV: E1 =:!= E2): mtl.Extends[Caio[E1, V, L, *], E1, E2] = ???

  def applicativeAsk:ApplicativeAsk[Caio[E1, V, L, *], E1] = ???

  def monadState:MonadState[Caio[E1, V, L, *], E1] = ???
}

class CaioProvides[V, L:Monoid, E] extends Provides[Caio[Unit, V, L, *], E] {

  type FE[A] = Caio[E, V, L, A]

  def apply[A](e:E)(f:Caio[E, V, L, A]):Caio[Unit, V, L, A] =
    CaioKleisli[Unit, V, L, A]{_ =>
      IOResult{
        f.eval(e).map {
          case (Left(e), _, l) =>
            ErrorResult(e, LogStore(l))
          case (Right(a), _, l) =>
            SuccessResult(a, LogStore(l))
        }
      }
    }

  def applicativeAsk[E2](M: Mixer[E, E2]): ApplicativeAsk[FE, E2] = ???

  def monadState[E2](M: Mixer[E, E2]): MonadState[FE, E2] = ???

  def extender[E2](M: Mixer[E, E2]): mtl.Extender[FE, E] = ???
}

trait Extends[F[_], E1, E2] extends Askable[F, (E1, E2)] {

  def apply[A](c:E2)(f:FE[A]):F[A]
}


object Provider {
  def apply[F[_]](implicit P:Provider[F]):Provider[F] = P
}