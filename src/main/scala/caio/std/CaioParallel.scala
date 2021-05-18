package caio.std

import caio.{Caio, ParCaio}
import cats.{~>, CommutativeApplicative, Monad, Monoid, Parallel}
import cats.effect.{ContextShift, IO}

trait CaioNewtype {
  type Base
  trait Tag extends Any

  type Type[C, V, L, +A] <: Base with Tag

  def apply[C, V, L, A](caio: Caio[C, V, L, A]): Type[C, V, L, A] =
    caio.asInstanceOf[Type[C, V, L, A]]

  def unwrap[C, V, L, A](`type`: Type[C, V, L, A]): Caio[C, V, L, A] =
    `type`.asInstanceOf[Caio[C, V, L, A]]
}

object Par extends CaioNewtype

class CaioParallel[C, V, L: Monoid](implicit CS: ContextShift[IO])
    extends CaioConcurrent[C, V, L]
    with Parallel[Caio[C, V, L, *]] {
  override type F[A] = ParCaio[C, V, L, A]

  override val applicative: CommutativeApplicative[ParCaio[C, V, L, *]] =
    new CaioParApplicative[C, V, L](this)

  override val monad: Monad[Caio[C, V, L, *]] =
    new CaioMonad[C, V, L]

  override val sequential: ParCaio[C, V, L, *] ~> Caio[C, V, L, *] =
    new (ParCaio[C, V, L, *] ~> Caio[C, V, L, *]) {
      def apply[A](fa: ParCaio[C, V, L, A]): Caio[C, V, L, A] = Par.unwrap(fa)
    }

  override val parallel: Caio[C, V, L, *] ~> ParCaio[C, V, L, *] =
    new (Caio[C, V, L, *] ~> ParCaio[C, V, L, *]) {
      def apply[A](fa: Caio[C, V, L, A]): ParCaio[C, V, L, A] = Par(fa)
    }
}
