package caio.mtl

import cats.{Monad, MonadError}
import cats.effect.{Async, Bracket, LiftIO, Sync}
import cats.mtl._

trait Context[F[_], L, E] {

  def apply[C]:EnvironmentContext[F, L, E, C]
}


trait EnvironmentContext[F[_], L, E, C] {

  type F2[A]

  implicit def AA:ApplicativeAsk[F2, C]

  implicit def monadErrorMap(implicit ME:MonadError[F2, E]):MonadError[F2, E]

  implicit def syncMap(implicit S:Sync[F]):Sync[F2]

  implicit def monadMap(implicit M:Monad[F]):Monad[F2]

  implicit def asyncMap(implicit A:Async[F]):Async[F2]

  implicit def bracketMap(implicit A:Bracket[F, E]):Bracket[F2, E]

  implicit def liftIOMap(implicit L:LiftIO[F]):LiftIO[F2]

  def apply[A](c:C)(f:F2[A]):F[A]
}


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