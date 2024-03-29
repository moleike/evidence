package evidence
package internal

import cats.Alternative
import cats.Foldable
import cats.Monad
import cats.MonoidK
import cats.implicits._

trait NonDet[E, Ans]:
  def empty: Op[Unit, Nothing, E, Ans]
  def choose: Op[Unit, Boolean, E, Ans]

object NonDet:
  def choose[E]: NonDet :? E ?=> Eff[E, Boolean] =
    Eff.perform[Unit, Boolean, E, NonDet](
      [EE, Ans] => (_: NonDet[EE, Ans]).choose
    )(())

  def empty[E]: NonDet :? E ?=> Eff[E, Nothing] =
    Eff.perform[Unit, Nothing, E, NonDet](
      [EE, Ans] => (_: NonDet[EE, Ans]).empty
    )(())

  given [E](using NonDet :? E, Monad[Eff[E, *]]): Alternative[Eff[E, *]] with {
    def empty[A]: Eff[E, A]      = NonDet.empty
    def pure[A](x: A): Eff[E, A] = Monad[Eff[E, *]].pure(x)
    def ap[A, B](ff: Eff[E, A => B])(fa: Eff[E, A]): Eff[E, B] =
      Monad[Eff[E, *]].ap(ff)(fa)
    def combineK[A](x: Eff[E, A], y: Eff[E, A]): Eff[E, A] =
      NonDet.choose.ifM(x, y)
  }

  def choice[E, A, G[_]](
      ps: G[Eff[E, A]]
  )(using G: Foldable[G], F: MonoidK[Eff[E, *]]): Eff[E, A] = G.foldK(ps)

  def amb[E, A, F[_]](using
      F: Alternative[F]
  ): Eff[NonDet :* E, A] => Eff[E, F[A]] =
    Eff.handlerRet(
      F.pure[A](_),
      new NonDet[E, F[A]]:
        def empty = Op.except(Function.const(F.empty.pure))
        def choose = Op((_, k) =>
          for
            xs <- k(true)
            ys <- k(false)
          yield xs <+> ys
        )
      ,
      _
    )

  def allResults[E, A]: Eff[NonDet :* E, A] => Eff[E, List[A]] = amb[E, A, List]

  def firstResult[E, A]: Eff[NonDet :* E, A] => Eff[E, Option[A]] =
    amb[E, A, Option]
