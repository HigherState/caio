package caio.mtl

import caio.{Caio, CaioState, EventLog, State}
import cats.Functor
import cats.mtl.FunctorTell

trait CaioFunctorTell extends FunctorTell[Caio, EventLog]{
  override lazy val functor: Functor[Caio] =
    new CaioFunctor {}

  def tell(l: EventLog): Caio[Unit] =
    CaioState((), State(l))

  def writer[A](a: A, l: EventLog): Caio[A] =
    CaioState(a, State(l))

  def tuple[A](ta: (EventLog, A)): Caio[A] =
    writer(ta._2, ta._1)
}
