package evidence
package effect

import cats.Alternative
import cats.Monad
import cats.implicits._
import evidence.Ctx.In

type NonDet = [E, Ans] =>> NonDet.Syn[E, Ans]

object NonDet:
  trait Syn[E, Ans]:
    def empty: Op[Unit, Nothing, E, Ans]
    def choose: Op[Unit, Boolean, E, Ans]

  def choose[E]: NonDet :? E ?=> Eff[E, Boolean] =
    Eff.perform[Unit, Boolean, E, NonDet](
      [EE, Ans] => (_: NonDet[EE, Ans]).choose
    )(())

  def empty[E]: NonDet :? E ?=> Eff[E, Nothing] =
    Eff.perform[Unit, Nothing, E, NonDet](
      [EE, Ans] => (_: NonDet[EE, Ans]).empty
    )(())

  given [E](using In[NonDet, E], Monad[Eff[E, *]]): Alternative[Eff[E, *]]
  with {
    def empty[A]: Eff[E, A] = NonDet.empty
    def pure[A](x: A): Eff[E, A] = Monad[Eff[E, *]].pure(x)
    def ap[A, B](ff: Eff[E, A => B])(fa: Eff[E, A]): Eff[E, B] =
      Monad[Eff[E, *]].ap(ff)(fa)
    def combineK[A](x: Eff[E, A], y: Eff[E, A]): Eff[E, A] =
      NonDet.choose.ifM(x, y)
  }

  def allResults[E, A, F[_]](using
      F: Alternative[F]
  ): Eff[NonDet :* E, A] => Eff[E, F[A]] =
    Eff.handlerRet(
      F.pure[A](_),
      new Syn[E, F[A]]:
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
