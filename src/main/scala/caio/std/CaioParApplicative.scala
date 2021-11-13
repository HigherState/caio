package caio.std

import caio.{Caio, ParCaio}
import cats.{CommutativeApplicative, Monoid}
import cats.effect.IO

class CaioParApplicative[C, V, L: Monoid](C: CaioConcurrent[C, V, L])(implicit CS: ContextShift[IO])
    extends CommutativeApplicative[ParCaio[C, V, L, *]] {

  override def pure[A](x: A): ParCaio[C, V, L, A] =
    Par(Caio.pure(x))

  override def map2[A, B, D](fa: ParCaio[C, V, L, A], fb: ParCaio[C, V, L, B])(f: (A, B) => D): ParCaio[C, V, L, D] =
    Par {
      C.racePair(Par.unwrap(fa), Par.unwrap(fb)).flatMap {
        case Left((a, fiberB))  =>
          fiberB.join.map(b => f(a, b))
        case Right((fiberA, b)) =>
          fiberA.join.map(a => f(a, b))
      }
    }

  override def ap[A, B](ff: ParCaio[C, V, L, A => B])(fa: ParCaio[C, V, L, A]): ParCaio[C, V, L, B] =
    map2(ff, fa)(_(_))

  override def product[A, B](fa: ParCaio[C, V, L, A], fb: ParCaio[C, V, L, B]): ParCaio[C, V, L, (A, B)] =
    map2(fa, fb)((_, _))

  override def map[A, B](fa: ParCaio[C, V, L, A])(f: A => B): ParCaio[C, V, L, B] =
    Par(Par.unwrap(fa).map(f))

  override def unit: ParCaio[C, V, L, Unit] =
    pure(())
}
