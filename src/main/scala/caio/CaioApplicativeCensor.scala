package caio

import cats.{Applicative, Monoid}
import cats.mtl.ApplicativeCensor

class CaioApplicativeCensor extends ApplicativeCensor[Caio, EventLog] {
  val applicative: Applicative[Caio] =
    new CaioMonad {}
  val monoid: Monoid[EventLog] =
    Monoid.instance(Vector.empty, _ ++ _)

  def censor[A](fa: Caio[A])(f: EventLog => EventLog): Caio[A] =


  def clear[A](fa: Caio[A]): Caio[A] = ???

  def listen[A](fa: Caio[A]): Caio[(A, EventLog)] = ???

  def listens[A, B](fa: Caio[A])(f: EventLog => B): Caio[(A, B)] = ???

  def tell(l: EventLog): Caio[Unit] = ???

  def writer[A](a: A, l: EventLog): Caio[A] = ???

  def tuple[A](ta: (EventLog, A)): Caio[A] = ???
}
