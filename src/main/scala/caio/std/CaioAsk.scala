package caio.std

import caio.Caio
import caio.mtl.InvariantAsk
import cats.mtl.{Local, Stateful}

trait CaioAsk[C, L] extends InvariantAsk[Caio[C, L, *], C] {

  def ask[C1 >: C]: Caio[C, L, C1] =
    Caio.getContext

  override def reader[A](f: C => A): Caio[C, L, A] =
    Caio.getContext.map(f)
}

trait CaioLocal[C, L] extends CaioAsk[C, L] with Local[Caio[C, L, *], C] {

  def local[A](fa: Caio[C, L, A])(f: C => C): Caio[C, L, A] =
    fa.localContext(f)
}

trait CaioStateful[C, L] extends Stateful[Caio[C, L, *], C] {

  def get: Caio[C, L, C] =
    Caio.getContext

  def set(s: C): Caio[C, L, Unit] =
    Caio.setContext(s)

  override def inspect[A](f: C => A): Caio[C, L, A] =
    Caio.getContext.map(f)

  override def modify(f: C => C): Caio[C, L, Unit] =
    Caio.modifyContext(f)
}

