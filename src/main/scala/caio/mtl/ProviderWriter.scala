package caio.mtl

import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

trait ProviderWriter[F[_], L] {
  def apply[E]:ProvidesWriter[F, L, E]
}

trait ExtenderWriter[F[_], L, E1] {
  def apply[E2](implicit M:Mixer[(E1, E2), E2], EV: E1 =:!= E2):ExtendsWriter[F, L, E1, E2]

  def applicativeAsk:ApplicativeAsk[F, E1]

  def monadState:MonadState[F, E1]
}

trait ProvidesWriter[F[_], L, E] extends Askable[F, E] with ContextTransformers[F] with ContextWriterTransformers[F, L] {

  def apply[A](e:E)(f:FE[A]):F[A]
}

trait ExtendsWriter[F[_], L, E1, E2] extends Askable[F, (E1, E2)] with ContextTransformers[F] with ContextWriterTransformers[F, L]  {

  def apply[A](c:E2)(f:FE[A]):F[A]
}