package caio.mtl

import caio.{Caio, EventLog, State}
import cats.mtl.ApplicativeCensor
import cats.{Functor, Monoid}

trait CaioApplicativeCensor extends ApplicativeCensor[Caio, EventLog] with CaioFunctorListen {

  val monoid: Monoid[EventLog] =
    Monoid.instance(Vector.empty, _ ++ _)

  def censor[A](fa: Caio[A])(f: EventLog => EventLog): Caio[A] =
    fa.mapState{s =>
      State(s.content, f(s.eventLog))
    }

  def clear[A](fa: Caio[A]): Caio[A] =
    censor(fa)(_ => EventLog.empty)
}
