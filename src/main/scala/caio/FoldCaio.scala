package caio

import cats.effect.IO

sealed trait FoldCaio[C, L, +A] {

  /**
   * Required for transforming context outside of the evaluation GADT
   * Can transform Error and Failed cases as well
   * @param f
   * @tparam C2
   * @return
   */
  def contextMap[C2](f: C => C2): FoldCaio[C2, L, A]

  def flatMap[B](f: (C, Option[L], A) => FoldCaio[C, L, B]): FoldCaio[C, L, B]

  def toIO: IO[A]

  def map[B](f: A => B): FoldCaio[C, L, B]

  /**
   * Required for transforming EventLog, cant use FlatMap
   * Can transform Error and Failed cases as well
   * @param f
   * @return
   */
  def mapL[B](f: L => L): FoldCaio[C, L, A]
}

sealed trait FoldCaioPure[C, L, +A] extends FoldCaio[C, L, A] {

  def c: C

  def opt: Option[L]

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, A]

  def map[B](f: A => B): FoldCaioPure[C, L, B]

  def flatMap[B](f: (C, Option[L], A) => FoldCaio[C, L, B]): FoldCaio[C, L, B]

  def mapL[B](f: L => L): FoldCaioPure[C, L, A]
}

final private[caio] case class FoldCaioSuccess[C, L, +A](c: C, opt: Option[L], a: A) extends FoldCaioPure[C, L, A] {

  def toIO: IO[A] =
    IO.pure(a)

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, A] =
    this.copy(c = f(this.c))

  def map[B](f: A => B): FoldCaioPure[C, L, B] =
    this.copy(a = f(a))

  def flatMap[B](f: (C, Option[L], A) => FoldCaio[C, L, B]): FoldCaio[C, L, B] =
    f(c, opt, a)

  def mapL[B](f: L => L): FoldCaioPure[C, L, A] =
    this.copy(opt = opt.map(f))
}

final private[caio] case class FoldCaioError[C, L, +A](c: C, opt: Option[L], e: Throwable)
    extends FoldCaioPure[C, L, Nothing] {

  def toIO: IO[Nothing] =
    IO.raiseError(e)

  def contextMap[C2](f: C => C2): FoldCaioPure[C2, L, Nothing] =
    this.copy(c = f(this.c))

  def map[B](f: Nothing => B): FoldCaioPure[C, L, B] =
    this

  def flatMap[B](f: (C, Option[L], Nothing) => FoldCaio[C, L, B]): FoldCaio[C, L, B] =
    this

  def mapL[B](f: L => L): FoldCaioPure[C, L, Nothing] =
    this.copy(opt = opt.map(f))
}

final private[caio] case class FoldCaioIO[C, L, +A](io: IO[FoldCaioPure[C, L, A]]) extends FoldCaio[C, L, A] {
  def contextMap[C2](f: C => C2): FoldCaioIO[C2, L, A] =
    FoldCaioIO[C2, L, A](io.map(_.contextMap(f)))

  def flatMap[B](f: (C, Option[L], A) => FoldCaio[C, L, B]): FoldCaio[C, L, B] =
    FoldCaioIO {
      io.flatMap(_.flatMap(f) match {
        case FoldCaioIO(io2)          =>
          io2
        case p: FoldCaioPure[C, L, B] =>
          IO.pure(p)
      })
    }

  def map[B](f: A => B): FoldCaio[C, L, B] =
    FoldCaioIO(io.map(_.map(f)))

  def mapL[B](f: L => L): FoldCaio[C, L, A] =
    FoldCaioIO(io.map(_.mapL(f)))

  def toIO: IO[A] =
    io.flatMap(_.toIO)
}
