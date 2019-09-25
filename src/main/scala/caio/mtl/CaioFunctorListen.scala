package caio.mtl

import caio.{Caio, EventLog}
import cats.mtl.FunctorListen

trait CaioFunctorListen extends FunctorListen[Caio, EventLog] with CaioFunctorTell {

  def listen[A](fa: Caio[A]): Caio[(A, EventLog)] =
    fa.mapWithState[(A, EventLog)]((a, s) => (a -> s.eventLog, s))

  def listens[A, B](fa: Caio[A])(f: EventLog => B): Caio[(A, B)] =
    fa.mapWithState[(A, B)]((a, s) => (a -> f(s.eventLog), s))

}
