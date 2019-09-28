package caio.mtl

import caio.{Caio, EventLog, Store}
import cats.mtl.ApplicativeCensor
import cats.{Applicative, Monoid}

trait CaioApplicativeCensor extends ApplicativeCensor[Caio, EventLog] with CaioFunctorListen {

  val applicative: Applicative[Caio] =
    new CaioApplicative {}

  val monoid: Monoid[EventLog] =
    Monoid.instance(Vector.empty, _ ++ _)

  def censor[A](fa: Caio[A])(f: EventLog => EventLog): Caio[A] =
    fa.mapStore{s =>
      Store(s.content, f(s.eventLog))
    }

  def clear[A](fa: Caio[A]): Caio[A] =
    censor(fa)(_ => EventLog.empty)
}
