package evidence

import cats.syntax.all._
import java.util.concurrent.atomic.AtomicReference
import evidence.Ctx.In
import Eff.given

type Local[A] = [E, Ans] =>> Local.Syn[A, E, Ans]

object Local:
  class Syn[A, E, Ans] private[Local] (private val ar: AtomicReference[A]) {
    def get: Op[Unit, A, E, Ans] = Op.function(_ => ar.get().pure)
    def put: Op[A, Unit, E, Ans] = Op.function(a => ar.set(a).pure *> ().pure)
  }

  def get[E, A](using In[Local[A], E]): Eff[E, A] =
    Eff.perform[Unit, A, E, Local[A]]([EE, Ans] => (_: Local[A][EE, Ans]).get)(
      ()
    )

  def put[E, A](a: A)(using In[Local[A], E]): Eff[E, Unit] =
    Eff.perform[A, Unit, E, Local[A]]([EE, Ans] => (_: Local[A][EE, Ans]).put)(
      a
    )

  def local[A, E, Ans](init: A): Eff[Local[A] :* E, Ans] => Eff[E, Ans] =
    Eff.handler(new Syn[A, E, Ans](AtomicReference(init)), _)
