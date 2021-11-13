package caio.std

import caio._
import cats.effect._

trait CaioConcurrent[C, L] extends CaioSpawn[C, L] with Concurrent[Caio[C, L, *]] {

  //to be provided in instance
  def sync:Sync[Caio[C, L, *]]

  def ref[A](a: A): Caio[C,  L, Ref[Caio[C, L, *], A]] =
    IOCaio(IO(Ref.unsafe(a)(sync)))

  // Implemented in CaioAsync
  def deferred[A]: Caio[C, L, Deferred[Caio[C, L, *], A]]
}
