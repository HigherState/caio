package caio.std

import caio.mtl.{Context, EnvironmentContext}
import caio.{Caio, CaioKleisli, ContentStore, ErrorResult, IOResult, SuccessResult}
import cats.Monoid
import cats.mtl.{ApplicativeAsk, MonadState}
import io.typechecked.alphabetsoup.Mixer
import shapeless.=:!=

class CaioContext[C, V, L:Monoid] extends Context[Caio[C, V, L, *], C, L, Throwable, V] {
  def apply[C2](implicit M:Mixer[C, C2], EV: C =:!= C2): EnvironmentContext[Caio[C, V, L, *], L, Throwable, V, C2] =
    new EnvironmentContext[Caio[C, V, L, *], L, Throwable, V, C2] {

      type FC[A] = Caio[C2, V, L, A]

      def applicativeAsk: ApplicativeAsk[FC, C2] = ???

      def monadState: MonadState[FC, C2] = ???

      def apply[A](c: C2)(f: FC[A]): Caio[C, V, L, A] =
        CaioKleisli[C, V, L, A]{c =>
          IOResult {
            f.eval(M.mix(c)).map {
              case (Left(e), c2, l) =>
                ErrorResult(e, ContentStore(M.inject(c2, c), l))
              case (Right(a), c2, l) =>
                SuccessResult(a, ContentStore(M.inject(c2, c), l))
            }
          }
        }
    }
}
