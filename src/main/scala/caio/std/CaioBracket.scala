package caio.std

import caio._
import cats.Monoid
import cats.data.NonEmptyList
import cats.effect.ExitCase.Error
import cats.effect.concurrent.Ref
import cats.effect.{Bracket, BracketThrow, ExitCase, IO }


class CaioBracket[C, V, L](implicit M:Monoid[L]) extends CaioMonadError[C, V, L] with Bracket[Caio[C, V, L, *], Throwable]  {

  val BT = implicitly[BracketThrow[IO]]

  private case class CaptureError(c:C, l:L, cause:Throwable) extends Throwable

  private case class CaptureFailure(c:C, l:L, head:V, tail:List[V]) extends Throwable

  type CRef = Ref[IO,L]
  def bracketCase[A, B](acquire: Caio[C, V, L, A])(use: A => Caio[C, V, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, V, L, Unit]): Caio[C, V, L, B] = {
    KleisliCaio { c =>
      Ref.of[IO, L](M.empty).flatMap { ref =>
        BT.bracketCase[FoldCaioPure[C, V, L, A], FoldCaioPure[C, V, L, B]](Caio.foldIO(acquire, c)) {
          case FoldCaioSuccess(c2, l, a) =>
            Caio.foldIO(use(a), c2).flatMap {
              case FoldCaioSuccess(c3, l2, b) =>
                IO.pure(FoldCaioSuccess[C, V, L, B](c3, M.combine(l, l2), b))
              case FoldCaioError(c3, l2, ex) =>
                IO.raiseError(CaptureError(c3, M.combine(l, l2), ex))
              case FoldCaioFailure(c3, l2, h, t) =>
                IO.raiseError(CaptureFailure(c3, M.combine(l, l2), h, t))
            }
          case failOrError =>
            IO.pure(failOrError.asInstanceOf[FoldCaioPure[C, V, L, B]])
        } {
          case (FoldCaioSuccess(c2, _, a), exitCase) =>
            val newExitCase = exitCase match {
              case Error(CaptureFailure(_, _, h, t)) =>
                Error(CaioUnhandledFailuresException(NonEmptyList(h, t)))
              case Error(CaptureError(_, _, ex)) =>
                Error(ex)
              case ec =>
                ec
            }
            Caio.foldIO(release(a, newExitCase), c2)
              .flatMap(p => ref.set(p.l).map(_ => p))
              .flatMap{
                case FoldCaioError(_, _, ex) =>
                  IO.raiseError(ex)
                case FoldCaioFailure(_, _, h, t) =>
                  IO.raiseError(CaioUnhandledFailuresException(NonEmptyList(h, t)))
                case _ =>
                  IO.unit
              }
          case _ =>
            IO.unit
        }.redeemWith(
          {
            case CaptureError(c2, l, ex) =>
              ref.get.map { l2 => FoldCaioError(c2, M.combine(l, l2), ex)}
            case CaptureFailure(c2, l, h, t) =>
              ref.get.map { l2 => FoldCaioFailure(c2, M.combine(l, l2), h, t)}
            case CaioUnhandledFailuresException(NonEmptyList(head:V, tail:List[V])) =>
              ref.get.map { l => FoldCaioFailure(c, l, head, tail)}
            case ex =>
              ref.get.map { l => FoldCaioError(c, l, ex)}
          },
          b =>ref.get.map { l2 => b.mapL(l => M.combine(l, l2)) }
        )
      }
    }
  }
}
