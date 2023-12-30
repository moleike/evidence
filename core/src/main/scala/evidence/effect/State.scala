package evidence
package effect

import cats.implicits._
import evidence.Ctx.In

type State[A] = [E, Ans] =>> State.Syn[A, E, Ans]

object State:
  trait Syn[A, E, Ans]:
    def get: Op[Unit, A, E, Ans]
    def put: Op[A, Unit, E, Ans]

  def apply[A]: Ops[A] = new Ops[A]

  // handler using the state-as-a-function representation
  def state[A, E, Ans](init: A): Eff[State[A] :* E, Ans] => Eff[E, (Ans, A)] =
    action =>
      for
        f <- Eff.handler(
          new Syn[A, E, A => Eff[E, (Ans, A)]]:
            val get = Op((_, k) => ((s: A) => k(s).flatMap(r => r(s))).pure)
            val put = Op((s, k) => ((_: A) => k(()).flatMap(r => r(s))).pure)
          ,
          for ans <- action
          yield (s => (ans, s).pure)
        )
        result <- f(init)
      yield result

  private[evidence] final class Ops[A](val dummy: Boolean = true) extends AnyVal:
    def get[E](using In[State[A], E]): Eff[E, A] =
      Eff.perform[Unit, A, E, State[A]](
        [EE, Ans] => (_: State[A][EE, Ans]).get
      )(())

    def put[E](a: A)(using In[State[A], E]): Eff[E, Unit] =
      Eff.perform[A, Unit, E, State[A]](
        [EE, Ans] => (_: State[A][EE, Ans]).put
      )(a)