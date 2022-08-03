package caio.std

import caio._
import cats.Monoid
import cats.effect.ExitCase.Error
import cats.effect.concurrent.Ref
import cats.effect.{Bracket, ExitCase, IO}

class CaioBracket[C, L](implicit M: Monoid[L]) extends CaioMonadError[C, L] with Bracket[Caio[C, L, *], Throwable] {

  private case class CaptureError(c: C, l: L, cause: Throwable) extends Throwable

  def bracketCase[A, B](
    acquire: Caio[C, L, A]
  )(use: A => Caio[C, L, B])(release: (A, ExitCase[Throwable]) => Caio[C, L, Unit]): Caio[C, L, B] =
    KleisliCaio[C, L, B] { c =>
      Ref.of[IO, L](M.empty).flatMap { ref =>
        Bracket[IO, Throwable]
          .bracketCase[FoldCaioPure[C, L, A], FoldCaioPure[C, L, B]](Caio.foldIO(acquire, c)) {
            case FoldCaioSuccess(acquireC, acquireL, a) =>
              Caio.foldIO(use(a), acquireC).flatMap {
                case FoldCaioSuccess(useC, useL, b) =>
                  IO.pure(FoldCaioSuccess[C, L, B](useC, M.combine(acquireL, useL), b))
                case FoldCaioError(useC, useL, ex)  =>
                  IO.raiseError(CaptureError(useC, M.combine(acquireL, useL), ex))
              }
            case failOrError                            =>
              IO.pure(failOrError.asInstanceOf[FoldCaioPure[C, L, B]])
          } {
            case (FoldCaioSuccess(acquireC, _, a), exitCase) =>
              val (newExitCase, forReleaseC) = exitCase match {
                case Error(CaptureError(useC, _, ex)) =>
                  Error(ex) -> useC
                case ec                               =>
                  ec -> acquireC
              }
              Caio
                .foldIO(release(a, newExitCase), forReleaseC)
                .flatMap(p => ref.set(p.l).map(_ => p))
                .flatMap {
                  case FoldCaioError(_, _, ex) =>
                    IO.raiseError(ex)
                  case _                       =>
                    IO.unit
                }
            case _                                           =>
              IO.unit
          }
          .redeemWith(
            {
              case CaptureError(useC, l, ex) =>
                ref.get.map(l2 => FoldCaioError(useC, M.combine(l, l2), ex))
              case ex                        =>
                ref.get.map(l => FoldCaioError(c, l, ex))
            },
            b => ref.get.map(l2 => b.mapL(l => M.combine(l, l2)))
          )
      }
    }
}
