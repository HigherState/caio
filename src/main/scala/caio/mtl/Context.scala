package caio.mtl

import cats.MonadError
import cats.effect.Sync

trait Context[F[_]] {

  //def apply[C]:EnvironmentContext[F, C]
}

//
//trait EnvironmentContext[F[_], C] {
//
//  type F2[A]
//
//  implicit def AA:ApplicativeAsk[F2, C]
//
//  implicit def mvMap(implicit MV:MonadValid[F]):MonadValid[F2]
//
//  implicit def meMap(implicit ME:MonadError[F2, Throwable]):MonadError[F2, Throwable]
//
//  implicit def sMap(implicit S:Sync[F]):Sync[F2]
//
//  implicit def mMap(implicit M:Monad[F]):Monad[F2]
//
//  def apply[A](c:C)(f:F2[A]):F[A]
//}
//
//
//class Testing[F[_]:MonadValid:Sync:Context]{
//  import FrameworkOps._
//
//  val CXT = implicitly[Context[F]]
//
//
//  def run:F[String] = {
//    implicit val C = CXT.apply[String]
//    import C._
//    C("hello")(internal[C.F2])
//  }
//
//
//  private def internal[F2[_]:ApplicativeAsk[?[_], String]:MonadValid:Sync]:F2[String] =
//    pure[F2, String]("")
//}