package caio.std

import caio.{Caio, CaioState, EventLog, Store}
import cats.Functor
import cats.mtl.FunctorTell

trait CaioFunctorTell extends FunctorTell[Caio, EventLog]{
  override lazy val functor: Functor[Caio] =
    new CaioFunctor {}

  def tell(l: EventLog): Caio[Unit] =
    CaioState((), Store(l))

  def writer[A](a: A, l: EventLog): Caio[A] =
    CaioState(a, Store(l))

  def tuple[A](ta: (EventLog, A)): Caio[A] =
    writer(ta._2, ta._1)
}
