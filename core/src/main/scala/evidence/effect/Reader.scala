package evidence
package effect

import cats.implicits._

import Ctx.*

type Reader[+A] = [E, Ans] =>> Reader.Syn[A, E, Ans]

object Reader:
  trait Syn[+A, E, Ans]:
    def ask: Op[Unit, A, E, Ans]

  def apply[A]: Ops[A] = new Ops[A]

  private[evidence] final class Ops[A](val dummy: Boolean = true)
      extends AnyVal:
    def ask[E](using In[Reader[A], E]): Eff[E, A] =
      Eff.perform[Unit, A, E, Reader[A]](
        [EE, Ans] => (_: Reader[A][EE, Ans]).ask
      )(())

    def local[E, Ans](
        f: A => A
    ): Eff[Reader[A] :* E, Ans] => Eff[Reader[A] :* E, Ans] =
      Eff.handlerHide(
        new Syn[A, Reader[A] :* E, Ans]:
          val ask = Op.function(_ => Reader[A].ask.map(f))
        ,
        _
      )

    def scope[E, Ans](
        a: A
    ): Eff[Reader[A] :* E, Ans] => Eff[Reader[A] :* E, Ans] = local(_ => a)

    def const[E, Ans](a: A): Eff[Reader[A] :* E, Ans] => Eff[E, Ans] =
      Eff.handler(
        new Syn[A, E, Ans]:
          val ask = Op.value(a)
        ,
        _
      )
