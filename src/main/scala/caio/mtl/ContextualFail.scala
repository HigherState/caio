package caio.mtl

trait ContextualFail {

  implicit def applicativeFailFE[F[_], FE[_], V](implicit P:Transform[FE, F], A:ApplicativeFail[F, V]):ApplicativeFail[FE, V] =
    P.transformApplicativeFail(A)
}
