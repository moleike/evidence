package evidence
package effect

import cats.Monoid
import cats.implicits._

type Writer[-A] = [E, Ans] =>> Writer.Syn[A, E, Ans]

object Writer:
  trait Syn[-A, E, Ans]:
    def tell: Op[A, Unit, E, Ans]

  def tell[A, E](a: A): Writer[A] :? E ?=> Eff[E, Unit] =
    Eff.perform[A, Unit, E, Writer[A]](
      [EE, Ans] => (r: Syn[A, EE, Ans]) => r.tell
    )(a)

  // TODO add listen as a Writer operation
  def listen[E, A, Ans]: (
      Monoid[A],
      Writer[A] :? E
  ) ?=> Eff[Writer[A] :* E, Ans] => Eff[E, (Ans, A)] = action =>
    writer(action).flatTap((a, w) => tell(w))

  def censor[E, A, Ans](f: A => A): (
      Monoid[A],
      Writer[A] :? E
  ) ?=> Eff[Writer[A] :* E, Ans] => Eff[E, Ans] = action =>
    writer(action).flatMap((a, w) => tell(f(w)).as(a))

  def writer[E, A, Ans]
      : Monoid[A] ?=> Eff[Writer[A] :* E, Ans] => Eff[E, (Ans, A)] = action =>
    State.state(Monoid[A].empty)(
      Eff.handlerHide(
        new Syn[A, State[A] :* E, Ans]:
          val tell =
            Op.function(x => State.get.flatMap(xs => State.put(xs.combine(x))))
        ,
        action
      )
    )

extension [E, L, A](eff: Eff[Writer[L] :* E, A])(using Monoid[L])
  def runW: Eff[E, (A, L)] = Writer.writer[E, L, A](eff)
  def censor(f: L => L): Writer[L] :? E ?=> Eff[E, A] = Writer.censor(f)(eff)
  def listen: Writer[L] :? E ?=> Eff[E, (A, L)] = Writer.listen(eff)
