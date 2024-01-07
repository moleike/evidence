package evidence
package effect

import cats.implicits._

trait Reader[+A, E, Ans]:
  def ask: Op[Unit, A, E, Ans]

object Reader:
  def apply[A]: Ops[A] = new Ops[A]

  private[evidence] final class Ops[A](val dummy: Boolean = true)
      extends AnyVal:
    def ask[E](using Ctx.In[Reader[A, *, *], E]): Eff[E, A] =
      Eff.perform[Unit, A, E, Reader[A, *, *]](
        [EE, Ans] => (_: Reader[A, EE, Ans]).ask
      )(())

    def local[E, Ans](
        f: A => A
    ): Eff[Reader[A, *, *] :* E, Ans] => Eff[Reader[A, *, *] :* E, Ans] =
      Eff.handlerHide(
        new Reader[A, Reader[A, *, *] :* E, Ans]:
          val ask = Op.function(_ => Reader[A].ask.map(f))
        ,
        _
      )

    def scope[E, Ans](
        a: A
    ): Eff[Reader[A, *, *] :* E, Ans] => Eff[Reader[A, *, *] :* E, Ans] =
      local(_ => a)

    def const[E, Ans](a: A): Eff[Reader[A, *, *] :* E, Ans] => Eff[E, Ans] =
      Eff.handler(
        new Reader[A, E, Ans]:
          val ask = Op.value(a)
        ,
        _
      )
