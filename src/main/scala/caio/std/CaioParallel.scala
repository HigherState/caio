package caio.std

import caio.{Caio, ParCaio}
import cats.{~>, CommutativeApplicative, Monad, Monoid, Parallel}
import cats.effect.{ContextShift, IO}

trait CaioNewtype {
  type Base
  trait Tag extends Any

  type Type[C, L, +A] <: Base with Tag

  def apply[C, L, A](caio: Caio[C, L, A]): Type[C, L, A] =
    caio.asInstanceOf[Type[C, L, A]]

  def unwrap[C, L, A](`type`: Type[C, L, A]): Caio[C, L, A] =
    `type`.asInstanceOf[Caio[C, L, A]]
}

object Par extends CaioNewtype

class CaioParallel[C, L: Monoid](implicit CS: ContextShift[IO])
    extends CaioConcurrent[C, L]
    with Parallel[Caio[C, L, *]] {
  override type F[A] = ParCaio[C, L, A]

  override val applicative: CommutativeApplicative[ParCaio[C, L, *]] =
    new CaioParApplicative[C, L](this)

  override val monad: Monad[Caio[C, L, *]] =
    new CaioMonad[C, L]

  override val sequential: ParCaio[C, L, *] ~> Caio[C, L, *] =
    new (ParCaio[C, L, *] ~> Caio[C, L, *]) {
      def apply[A](fa: ParCaio[C, L, A]): Caio[C, L, A] = Par.unwrap(fa)
    }

  override val parallel: Caio[C, L, *] ~> ParCaio[C, L, *] =
    new (Caio[C, L, *] ~> ParCaio[C, L, *]) {
      def apply[A](fa: Caio[C, L, A]): ParCaio[C, L, A] = Par(fa)
    }
}
