package caio.std

import caio._
import cats.Monoid
import cats.effect._

class CaioConcurrent[C, V, L:Monoid](implicit CS:ContextShift[IO]) extends CaioAsync[C, V, L] with Concurrent[Caio[C, V, L, *]] {

  def start[A](fa: Caio[C, V, L, A]): Caio[C, V, L, Fiber[Caio[C, V, L, *], A]] =
    fa.start

  def racePair[A, B](fa: Caio[C, V, L, A], fb: Caio[C, V, L, B]): Caio[C, V, L, Either[(A, Fiber[Caio[C, V, L, *], B]), (Fiber[Caio[C, V, L, *], A], B)]] =
    fa.racePair(fb)
}
