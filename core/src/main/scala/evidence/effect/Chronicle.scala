package evidence
package effect

import cats.kernel.Semigroup
import cats.data.Ior
import cats.implicits._

// An hybrid error/writer monad that allows both accumulating outputs and
// aborting computation with a final output.
type Chronicle[-A] = [E, Ans] =>> Chronicle.Syn[A, E, Ans]

object Chronicle:
  trait Syn[-A, E, Ans]:
    def dictate: Op[A, Unit, E, Ans]
    def confess: Op[A, Nothing, E, Ans]

  def confess[A, E](a: A): Chronicle[A] :? E ?=> Eff[E, Nothing] =
    Eff.perform[A, Nothing, E, Chronicle[A]](
      [E, Ans] => (e: Syn[A, E, Ans]) => e.confess
    )(a)

  def dictate[A, E](a: A): Chronicle[A] :? E ?=> Eff[E, Unit] =
    Eff.perform[A, Unit, E, Chronicle[A]](
      [EE, Ans] => (r: Syn[A, EE, Ans]) => r.dictate
    )(a)

  def materialize[A, Ans, E]: (
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
