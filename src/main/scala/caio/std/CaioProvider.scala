package caio.std

import caio.{<~>, Caio, KleisliCaio, mtl}
import caio.mtl._
import cats.effect._
import cats._
import cats.mtl._
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

case class CaioExtender[C, V, L:Monoid, E1](
  applicativeAsk: InvariantAsk[Caio[C, V, L, *], E1],
  state: Stateful[Caio[C, V, L, *], E1],
)(implicit M: Mixer[C, E1], I:Mixer[(E1, Unit), C]) extends Extender[Caio[C, V, L, *], E1] {

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[C, V, L, *], E1, E2] =
    CaioExtends[C, V, L, E1, E2]()

}

case class CaioExtendsOn[C, C0, V, L:Monoid, E1](
  applicativeAsk: InvariantAsk[Caio[C, V, L, *], E1],
  state: Stateful[Caio[C, V, L, *], E1],
  functionK: Caio[C0, V, L, *] ~> Caio[C, V, L, *]
)(implicit M: Mixer[C, E1], I:Mixer[(E1, Unit), C])
  extends ExtendsOn[Caio[C, V, L, *], Caio[C0, V, L, *], E1] {
  def bijectionK(e2: E1): Caio[C0, V, L, *] <~> Caio[C, V, L, *] = ???

  def apply[E2](implicit EV: E1 =:!= E2): mtl.Extends[Caio[C, V, L, *], E1, E2] =
    CaioExtends[C, V, L, E1, E2]()
}

case class CaioExtends[C, V, L:Monoid, E1, E2]()(implicit M: Mixer[C, E1], I:Mixer[(E1, Unit), C])
  extends Extends[Caio[C, V, L, *], E1, E2] {

  type FE[A] = Caio[(E1, E2), V, L, A]

  val ask: InvariantAsk[FE, (E1, E2)] =
    new CaioAsk[(E1, E2), V, L]

  val stateful: Stateful[FE, (E1, E2)] =
    new CaioStateful[(E1, E2), V, L]

  def extender[E3](implicit M: Mixer[(E1, E2), E3], I:Mixer[(E3, Unit), (E1, E2)]): ExtendsOn[FE, Caio[C, V, L, *], E3] =
    CaioExtendsOn[(E1, E2), C, V, L, E3](
      new MixedAsk[FE, (E1, E2), E3](ask),
      new MixedStateful[FE, (E1, E2), E3](stateful),
      functionK
    )

  def functionK: Caio[C, V, L, *] ~> FE =
    new (Caio[C, V, L, *] ~> FE) {
      def apply[A](fa: Caio[C, V, L, A]): Caio[(E1, E2), V, L, A] =
        KleisliCaio[(E1, E2), V, L, A] { c2 =>
          Caio.foldIO[C, V, L, A](fa, I.mix(c2._1 -> ())).map(_.contextMap(c => M.mix(c) -> c2._2))
        }
    }

  def apply(e2: E2): FE <~> Caio[C, V, L, *] =
    new CaioBijectionK[(E1, E2), C, V, L](c => M.mix(c) -> e2, p => I.mix(p._1 -> ()))

  implicit def transformApplicative(implicit A:Applicative[Caio[C, V, L, *]]):Applicative[FE] =
    A.asInstanceOf[Applicative[FE]]

  implicit def transformFunctor(implicit F:Functor[Caio[C, V, L, *]]):Functor[FE] =
    F.asInstanceOf[Functor[FE]]

  implicit def transformMonad(implicit M:Monad[Caio[C, V, L, *]]):Monad[FE] =
    M.asInstanceOf[Monad[FE]]

  implicit def transformMonadError[E](implicit M:MonadError[Caio[C, V, L, *], E]):MonadError[FE, E] =
    M.asInstanceOf[MonadError[FE, E]]

  implicit def transformBracket[E](implicit M:Bracket[Caio[C, V, L, *], E]):Bracket[FE, E] =
    M.asInstanceOf[Bracket[FE, E]]

  implicit def transformSync(implicit S:Sync[Caio[C, V, L, *]]):Sync[FE] =
    S.asInstanceOf[Sync[FE]]

  implicit def transformAsync(implicit A:Async[Caio[C, V, L, *]]):Async[FE] =
    A.asInstanceOf[Async[FE]]

  implicit def transformLiftIO(implicit L:LiftIO[Caio[C, V, L, *]]):LiftIO[FE] =
    L.asInstanceOf[LiftIO[FE]]

  implicit def transformConcurrent(implicit C:Concurrent[Caio[C, V, L, *]]):Concurrent[FE] =
    C.asInstanceOf[Concurrent[FE]]

  implicit def transformApplicativeFail[V2](implicit A:ApplicativeFail[Caio[C, V, L, *], V2]):ApplicativeFail[FE, V2] =
    A.asInstanceOf[ApplicativeFail[FE, V2]]

  implicit def transformTell[L2](implicit F: Tell[Caio[C, V, L, *], L2]): Tell[FE, L2] =
    F.asInstanceOf[Tell[FE, L2]]

  implicit def transformListen[L2](implicit F: Listen[Caio[C, V, L, *], L2]): Listen[FE, L2] =
    F.asInstanceOf[Listen[FE, L2]]

  implicit def transformCensor[L2](implicit F: Censor[Caio[C, V, L, *], L2]): Censor[FE, L2] =
    F.asInstanceOf[Censor[FE, L2]]
}


object CaioProvider {
  implicit val M:Mixer[Unit, Unit] = new Mixer[Unit, Unit] {
    override def mix(a: Unit): Unit = ()

    override def inject(b: Unit, a: Unit): Unit = ()
  }
  implicit def unit[V, L:Monoid]:Provider[Caio[Unit, V, L, *]] =
    CaioExtender[Unit, V, L, Unit](
      new CaioAsk[Unit, V, L],
      new CaioStateful[Unit, V, L]
    )
}