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

