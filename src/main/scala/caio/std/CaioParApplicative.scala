package caio.std

import caio.{Caio, ParCaio}
import cats.{CommutativeApplicative, Monoid}
import cats.effect.{ContextShift, IO}

class CaioParApplicative[C, L: Monoid](C: CaioConcurrent[C, L])(implicit CS: ContextShift[IO])
    extends CommutativeApplicative[ParCaio[C, L, *]] {

  override def pure[A](x: A): ParCaio[C, L, A] =
    Par(Caio.pure(x))

  override def map2[A, B, D](fa: ParCaio[C, L, A], fb: ParCaio[C, L, B])(f: (A, B) => D): ParCaio[C, L, D] =
    Par {
      C.racePair(Par.unwrap(fa), Par.unwrap(fb)).flatMap {
        case Left((a, fiberB))  =>
          fiberB.join.map(b => f(a, b))
        case Right((fiberA, b)) =>
          fiberA.join.map(a => f(a, b))
      }
    }

  override def ap[A, B](ff: ParCaio[C, L, A => B])(fa: ParCaio[C, L, A]): ParCaio[C, L, B] =
    map2(ff, fa)(_(_))

  override def product[A, B](fa: ParCaio[C, L, A], fb: ParCaio[C, L, B]): ParCaio[C, L, (A, B)] =
    map2(fa, fb)((_, _))

  override def map[A, B](fa: ParCaio[C, L, A])(f: A => B): ParCaio[C, L, B] =
    Par(Par.unwrap(fa).map(f))

  override def unit: ParCaio[C, L, Unit] =
    pure(())
}
