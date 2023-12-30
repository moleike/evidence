package evidence
package effect

import cats.kernel.Semigroup
import cats.data.Ior
import cats.implicits._
import evidence.Ctx.In

// An hybrid error/writer monad that allows both accumulating outputs and
// aborting computation with a final output.
type Chronicle[-A] = [E, Ans] =>> Chronicle.Syn[A, E, Ans]

object Chronicle:
  trait Syn[-A, E, Ans]:
    def dictate: Op[A, Unit, E, Ans]
    def confess: Op[A, Nothing, E, Ans]

  def apply[A]: Ops[A] = new Ops[A]

  private[evidence] final class Ops[A](val dummy: Boolean = true) extends AnyVal:
    def confess[E](a: A)(using In[Chronicle[A], E]): Eff[E, Nothing] =
      Eff.perform[A, Nothing, E, Chronicle[A]](
        [E, Ans] => (_: Chronicle[A][E, Ans]).confess
      )(a)

    def dictate[E](a: A)(using In[Chronicle[A], E]): Eff[E, Unit] =
      Eff.perform[A, Unit, E, Chronicle[A]](
        [EE, Ans] => (_: Chronicle[A][EE, Ans]).dictate
      )(a)

  def materialize[E, A, Ans]: (
      Semigroup[A],
      Semigroup[Ans]
  ) ?=> Eff[Chronicle[A] :* E, Ans] => Eff[E, A Ior Ans] =
    Eff.handlerRet(
      Ior.right[A, Ans](_),
      new Syn[A, E, A Ior Ans]:
        val dictate = Op((a, k) =>
          k(()).map {
            case l @ Ior.Left(_)  => l
            case Ior.Both(b, ans) => Ior.Both(a.combine(b), ans)
            case r @ Ior.Right(_) => r.addLeft(a)
          }
        )
        val confess = Op.except[A, E, A Ior Ans](Ior.left(_).pure)
      ,
      _
    )