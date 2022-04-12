package caio.std

import caio.{<~>, mtl, Caio}
import caio.mtl._
import cats.effect._
import cats._
import cats.mtl._
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

case class CaioExtender[C, L: Monoid, E1](
  applicativeAsk: InvariantAsk[Caio[C, L, *], E1],
  state: Stateful[Caio[C, L, *], E1]
)(implicit M: Mixer[C, E1], I: Mixer[(E1, Unit), C])
    extends Extender[Caio[C, L, *], E1] {

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[C, L, *], E1, E2] =
    CaioExtends[C, L, E1, E2]()

}

case class CaioExtendsOn[C, C0, L: Monoid, E1](
  applicativeAsk: InvariantAsk[Caio[C, L, *], E1],
  state: Stateful[Caio[C, L, *], E1],
  functionK: Caio[C0, L, *] ~> Caio[C, L, *]
)(implicit M: Mixer[C, E1], I: Mixer[(E1, Unit), C])
    extends ExtendsOn[Caio[C, L, *], Caio[C0, L, *], E1] {
  def bijectionK(e2: E1): Caio[C0, L, *] <~> Caio[C, L, *] = ???

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[C, L, *], E1, E2] =
    CaioExtends[C, L, E1, E2]()
}

case class CaioExtends[C, L: Monoid, E1, E2]()(implicit M: Mixer[C, E1], I: Mixer[(E1, Unit), C])
    extends Extends[Caio[C, L, *], E1, E2] {

  type FE[A] = Caio[(E1, E2), L, A]

  val ask: InvariantAsk[FE, (E1, E2)] =
    CaioAsk[(E1, E2), L]

  val stateful: Stateful[FE, (E1, E2)]                                                                                =
    new CaioStateful[(E1, E2), L] {}

  def extender[E3](implicit M: Mixer[(E1, E2), E3], I: Mixer[(E3, Unit), (E1, E2)]): ExtendsOn[FE, Caio[C, L, *], E3] =
    CaioExtendsOn[(E1, E2), C, L, E3](
      new MixedAsk[FE, (E1, E2), E3](ask),
      new MixedStateful[FE, (E1, E2), E3](stateful),
      functionK
    )

  def functionK: Caio[C, L, *] ~> FE =
    new (Caio[C, L, *] ~> FE) {
      def apply[A](fa: Caio[C, L, A]): Caio[(E1, E2), L, A] =
        Caio.KleisliCaio[(E1, E2), L, A] { (c2, ref) =>
          Caio.foldIO[C, L, A](fa, I.mix(c2._1 -> ()), ref).map(_.contextMap(c => M.mix(c) -> c2._2))
        }
    }

  def apply(e2: E2): FE <~> Caio[C, L, *]                                                    =
    new CaioBijectionK[(E1, E2), C, L](c => M.mix(c) -> e2, p => I.mix(p._1 -> ()))

  implicit def transformApplicative(implicit A: Applicative[Caio[C, L, *]]): Applicative[FE] =
    A.asInstanceOf[Applicative[FE]]

  implicit def transformFunctor(implicit F: Functor[Caio[C, L, *]]): Functor[FE] =
    F.asInstanceOf[Functor[FE]]

  implicit def transformMonad(implicit M: Monad[Caio[C, L, *]]): Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  implicit def transformMonadCancel(implicit M: MonadCancel[Caio[C, L, *], Throwable]): MonadCancel[FE, Throwable] =
    M.asInstanceOf[MonadCancel[FE, Throwable]]

  implicit def transformMonadError[E](implicit M: MonadError[Caio[C, L, *], E]): MonadError[FE, E] =
    M.asInstanceOf[MonadError[FE, E]]

  implicit def transformSync(implicit S: Sync[Caio[C, L, *]]): Sync[FE] =
    S.asInstanceOf[Sync[FE]]

  implicit def transformAsync(implicit A: Async[Caio[C, L, *]]): Async[FE] =
    A.asInstanceOf[Async[FE]]

  implicit def transformLiftIO(implicit L: LiftIO[Caio[C, L, *]]): LiftIO[FE] =
    L.asInstanceOf[LiftIO[FE]]

  implicit def transformConcurrent(implicit C: Concurrent[Caio[C, L, *]]): Concurrent[FE] =
    C.asInstanceOf[Concurrent[FE]]

  implicit def transformTell[L2](implicit F: Tell[Caio[C, L, *], L2]): Tell[FE, L2] =
    F.asInstanceOf[Tell[FE, L2]]

  implicit def transformListen[L2](implicit F: Listen[Caio[C, L, *], L2]): Listen[FE, L2] =
    F.asInstanceOf[Listen[FE, L2]]

  implicit def transformCensor[L2](implicit F: Censor[Caio[C, L, *], L2]): Censor[FE, L2] =
    F.asInstanceOf[Censor[FE, L2]]
}

object CaioProvider {
  implicit val M: Mixer[Unit, Unit]                        = new Mixer[Unit, Unit] {
    override def mix(a: Unit): Unit = ()

    override def inject(b: Unit, a: Unit): Unit = ()
  }
  implicit def unit[L: Monoid]: Provider[Caio[Unit, L, *]] =
    CaioExtender[Unit, L, Unit](CaioAsk[Unit, L], CaioStateful[Unit, L])
}
