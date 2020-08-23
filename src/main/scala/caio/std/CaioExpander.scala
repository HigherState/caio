package caio.std

import caio.{Caio, FoldCaioError, FoldCaioFailure, FoldCaioIO, FoldCaioSuccess, KleisliCaio, PureCaio}
import caio.mtl._
import cats.effect._
import cats._
import cats.data.NonEmptyList
import cats.mtl._

class CaioExpander[A, V, L:Monoid] extends Expander[Caio[A, V, L, *], A] {

  def expand[B]: Expanded[Caio[A, V, L, *], A, B] =
    new CaioExpanded[A, B, V, L]
}

class CaioExpanded[A, B, V, L:Monoid] extends Expanded[Caio[A, V, L, *], A, B] { self =>
  type M2[T] = Caio[(A, B), V, L, T]

  implicit def expands: Expands[Caio[(A, B), V, L, *], Caio[A, V, L, *], B] with Expander[Caio[(A,B), V, L, *], (A, B)] =
    new Expands[Caio[(A, B), V, L, *], Caio[A, V, L, *], B] with Expander[Caio[(A, B), V, L, *], (A, B)] {
      def apply[T](c: B, m: Caio[(A, B), V, L, T]): Caio[A, V, L, T] =
          self.apply(c)(m)

      def lift[T](m: Caio[A, V, L, T]): Caio[(A, B), V, L, T] =
        KleisliCaio[(A, B), V, L, T] { ab =>
          FoldCaioIO {
            m.runContext(ab._1).map {
              case (a, l, Right(t)) =>
                FoldCaioSuccess(a -> ab._2, l, t)
              case (a, l, Left(Left(ex))) =>
                FoldCaioError(a -> ab._2, l, ex)
              case (a, l, Left(Right(NonEmptyList(head, tail)))) =>
                FoldCaioFailure(a -> ab._2, l, head, tail)
            }
          }
        }

      def expand[B2]: Expanded[Caio[(A,B), V, L, *], (A, B), B2] =
        new CaioExpanded[(A, B), B2, V, L]

      def transformApplicative(A:Applicative[Caio[A, V, L, *]]):Applicative[M2] =
        A.asInstanceOf[Applicative[M2]]

      def transformFunctor(F:Functor[Caio[A, V, L, *]]):Functor[M2] =
        F.asInstanceOf[Functor[M2]]

      def transformMonad(M:Monad[Caio[A, V, L, *]]):Monad[M2] =
        M.asInstanceOf[Monad[M2]]

      def transformMonadError[E](M:MonadError[Caio[A, V, L, *], E]):MonadError[M2, E] =
        M.asInstanceOf[MonadError[M2, E]]

      def transformBracket[E](M:Bracket[Caio[A, V, L, *], E]):Bracket[M2, E] =
        M.asInstanceOf[Bracket[M2, E]]

      def transformSync(S:Sync[Caio[A, V, L, *]]):Sync[M2] =
        S.asInstanceOf[Sync[M2]]

      def transformAsync(A:Async[Caio[A, V, L, *]]):Async[M2] =
        A.asInstanceOf[Async[M2]]

      def transformLiftIO(L:LiftIO[Caio[A, V, L, *]]):LiftIO[M2] =
        L.asInstanceOf[LiftIO[M2]]

      def transformConcurrent(C:Concurrent[Caio[A, V, L, *]]):Concurrent[M2] =
        C.asInstanceOf[Concurrent[M2]]

      def transformApplicativeFail[V2](A:ApplicativeFail[Caio[A, V, L, *], V2]):ApplicativeFail[M2, V2] =
        A.asInstanceOf[ApplicativeFail[M2, V2]]

      def transformFunctorTell[L2](F:FunctorTell[Caio[A, V, L, *], L2]):FunctorTell[M2, L2] =
        F.asInstanceOf[FunctorTell[M2, L2]]

      def transformFunctorListen[L2](F:FunctorListen[Caio[A, V, L, *], L2]):FunctorListen[M2, L2] =
        F.asInstanceOf[FunctorListen[M2, L2]]

      def transformApplicativeCensor[L2](F:ApplicativeCensor[Caio[A, V, L, *], L2]):ApplicativeCensor[M2, L2] =
        F.asInstanceOf[ApplicativeCensor[M2, L2]]

      def transformClock(C: Clock[Caio[A, V, L, *]]): Clock[M2] =
        C.asInstanceOf[Clock[M2]]

      def transformTimer(T: Timer[Caio[A, V, L, *]]): Timer[M2] =
        T.asInstanceOf[Timer[M2]]

      def transformEffectful(E: Effectful[Caio[A, V, L, *]]): Effectful[Caio[(A, B), V, L, *]] = ???
    }

  def apply[T](b: B)(m: Caio[(A, B), V, L, T]): Caio[A, V, L, T] =
    KleisliCaio[A, V, L, T]{a =>
      FoldCaioIO {
        m.runContext(a -> b).map {
          case (ab, l, Right(t)) =>
            FoldCaioSuccess(ab._1, l, t)
          case (ab, l, Left(Left(ex))) =>
            FoldCaioError(ab._1, l, ex)
          case (ab, l, Left(Right(NonEmptyList(head, tail)))) =>
            FoldCaioFailure(ab._1, l, head, tail)
        }
      }
    }
}

