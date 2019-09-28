package caio.mtl

import caio._
import cats.Monad
import cats.mtl.MonadState

import scala.reflect.ClassTag

class CaioMonadState[S](implicit T:ClassTag[S]) extends MonadState[Caio, S] {
  val monad: Monad[Caio] =
    new CaioMonad {}

  def get: Caio[S] =
    CaioKleisli{c =>
      c.get(T).fold[PureResult[S]](ErrorResult(ContextNotFoundException(T), Store.empty)){ e =>
        SuccessResult(e, Store.empty)
      }
    }

  def set(s: S): Caio[Unit] =
    CaioState((), Store.empty.set(s))

  def inspect[A](f: S => A): Caio[A] =
    get.map(f)

  def modify(f: S => S): Caio[Unit] =
    get.flatMap(s => set(f(s)))
}
