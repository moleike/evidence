package evidence
package effect

import cats.implicits._

type NonDet = [E, Ans] =>> NonDet.Syn[E, Ans]

object NonDet:
  trait Syn[E, Ans]:
    def empty: Op[Unit, Nothing, E, Ans]
    def choose: Op[Unit, Boolean, E, Ans]

  def choose[E]: NonDet :? E ?=> Eff[E, Boolean] =
    Eff.perform[Unit, Boolean, E, NonDet](
      [EE, Ans] => (e: Syn[EE, Ans]) => e.choose
    )(())

  def empty[E]: NonDet :? E ?=> Eff[E, Nothing] =
    Eff.perform[Unit, Nothing, E, NonDet](
      [EE, Ans] => (e: Syn[EE, Ans]) => e.empty
    )(())

  def allResults[E, Ans]: Eff[NonDet :* E, Ans] => Eff[E, Seq[Ans]] =
    Eff.handlerRet(
      (x: Ans) => Seq(x),
      new Syn[E, Seq[Ans]]:
        def empty = Op.except(Function.const(Nil.pure))
        def choose = Op((_, k) =>
          for
            xs <- k(false)
            ys <- k(true)
          yield xs ++ ys
        )
      ,
      _
    )
