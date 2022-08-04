//package caio.std
//
//import caio.mtl.{AskProjection, Context, ContextApplicator}
//import caio.{Caio, CaioKleisli, ContentStore, ErrorResult, IOResult, SuccessResult}
//import cats.Monoid
//import cats.mtl.{ApplicativeAsk, MonadState}
//import io.typechecked.alphabetsoup.Mixer
//import shapeless.=:!=
//
//class CaioContext[C, L:Monoid] extends Context[Caio[C, L, *], C, L, Throwable, V] {
//  current =>
//
//  def apply[C2](implicit M:Mixer[(C, C2), C2], EV: C =:!= C2): ContextApplicator[Caio[C, L, *], L, Throwable, C2] =
//    new ContextApplicator[Caio[C, L, *], L, Throwable, C2] {
//
//      type FC[A] = Caio[(C, C2), L, A]
//
//      val applicativeAsk: ApplicativeAsk[FC, C2] =
//        new AskProjection[FC, (C, C2), C2](new CaioApplicativeAsk[(C, C2), L], M)
//
//      val monadState: MonadState[FC, C2] = ???
//
//      lazy val context: Context[FC, C2, L, Throwable, V] =
//        new NestedCaioContext[C, C2, L](current)
//
//      def apply[A](c2: C2)(f: FC[A]): Caio[C, L, A] =
//        CaioKleisli[C, L, A]{c =>
//          IOResult {
//            f.eval(M.mix(c -> c2)).map {
//              case (Left(e), c2, l) =>
//                ErrorResult(e, ContentStore(M.inject(c2, c -> c2)._1, l))
//              case (Right(a), c2, l) =>
//                SuccessResult(a, ContentStore(M.inject(c2, c -> c2)._1, l))
//            }
//          }
//        }
//    }
//}
//
//private class NestedCaioContext[C, C2, L:Monoid](private[std] val parent:Context[Caio[C, L, *], C, L, Throwable, V]) extends Context[Caio[C2, L, *], C2, L, Throwable, V] {
//  current =>
//
//  def apply[C3](implicit M: Mixer[(C2, C3), C3], EV: C2 =:!= C3): ContextApplicator[Caio[C2, L, *], L, Throwable, C3] =
//    new ContextApplicator[Caio[C2, L, *], L, Throwable, C3] {
//
//      type FC[A] = Caio[C3, L, A]
//
//      val applicativeAsk: ApplicativeAsk[FC, C3] = ???
//
//      val monadState: MonadState[FC, C3] = ???
//
//      val context: Context[FC, C3, L, Throwable, V] =
//        new NestedCaioContext[C2, C3, L](current)
//
//      def apply[A](c3: C3)(f: FC[A]): Caio[C2, L, A] =
//        CaioKleisli[C2, L, A]{c2 =>
//          IOResult {
//            f.eval(M.mix(c2 -> c3)).map {
//              case (Left(e), c3, l) =>
//                ErrorResult(e, ContentStore(M.inject(c3, c2 -> c3)._1, l))
//              case (Right(a), c3, l) =>
//                SuccessResult(a, ContentStore(M.inject(c3, c2 -> c3)._1, l))
//            }
//          }
//        }
//    }
//}
