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

  type CRef = Ref[IO,L]
  def bracketCase[A, B](acquire: Caio[C, V, L, A])(use: A => Caio[C, V, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, V, L, Unit]): Caio[C, V, L, B] = {
    KleisliCaio[C, V, L, B] { c =>
      Ref.of[IO, L](M.empty).flatMap { ref =>
        BT.bracketCase[FoldCaioPure[C, V, L, A], FoldCaioPure[C, V, L, B]](Caio.foldIO(acquire, c)) {
          case FoldCaioSuccess(acquireC, acquireL, a) =>
            Caio.foldIO(use(a), acquireC).flatMap {
              case FoldCaioSuccess(useC, useL, b) =>
                IO.pure(FoldCaioSuccess[C, V, L, B](useC, M.combine(acquireL, useL), b))
              case FoldCaioError(useC, useL, ex) =>
                IO.raiseError(CaptureError(useC, M.combine(acquireL, useL), ex))
              case FoldCaioFailure(useC, useL, h, t) =>
                IO.pure(FoldCaioFailure[C, V, L, B](useC, M.combine(acquireL, useL), h, t))
            }
          case failOrError =>
            IO.pure(failOrError.asInstanceOf[FoldCaioPure[C, V, L, B]])
        } {
          case (FoldCaioSuccess(acquireC, _, a), exitCase) =>
            val newExitCase = exitCase match {
              case Error(CaptureError(_, _, ex)) =>
                Error(ex)
              case ec =>
                ec
            }
            Caio.foldIO(release(a, newExitCase), acquireC)
              .flatMap(p => ref.set(p.l).map(_ => p))
              .flatMap{
                case FoldCaioError(_, _, ex) =>
                  IO.raiseError(ex)
                case _ =>
                  IO.unit
              }
          case _ =>
            IO.unit
        }.redeemWith(
          {
            case CaptureError(useC, acquireUsel, ex) =>
              ref.get.map { releaseL => FoldCaioError(useC, M.combine(acquireUsel, releaseL), ex)}
            case CaioUnhandledFailuresException(NonEmptyList(head:V@unchecked, tail:List[V])) =>
              ref.get.map { releaseL => FoldCaioFailure(c, releaseL, head, tail)}
            case ex =>
              ref.get.map { releaseL => FoldCaioError(c, releaseL, ex)}
          },
          b => ref.get.map { releaseL => b.mapL(l => M.combine(l, releaseL)) }
        )
      }
    }
  }
}
