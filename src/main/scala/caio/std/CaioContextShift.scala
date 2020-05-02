package caio.std

import caio._
import cats.Monoid
import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

class CaioContextShift[C, V, L:Monoid](implicit CS:ContextShift[IO]) extends ContextShift[Caio[C, V, L, *]] {

  def evalOn[A](ec: ExecutionContext)(fa: Caio[C, V, L, A]): Caio[C, V, L, A] =
    fa match {
      case k:CaioKleisli[C, V, L, A] =>
        CaioKleisli{c => //evaluate function
          IOResult {
            CS.evalOn(ec)(k.eval(c)).map {
              case (Right(a), c, l)  =>
                SuccessResult(a, ContentStore(c, l, implicitly[Monoid[L]]))
              case (Left(eof), c, l)   =>
                ErrorResult(eof, ContentStore(c, l, implicitly[Monoid[L]]))
            }
          }
        }
      case c => c
    }

  def shift: Caio[C, V, L, Unit] =
    CaioKleisli{ c =>
      IOResult(CS.shift.attempt.map{
        case Left(e) =>
          ErrorResult(Left(e), ContentStore(c, Monoid.empty[L], implicitly[Monoid[L]]))
        case Right(_) => SuccessResult((), ContentStore(c, Monoid.empty[L], implicitly[Monoid[L]]))
      })
    }
}
