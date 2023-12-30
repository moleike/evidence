package evidence
package effect

import cats.implicits._
import evidence.Ctx.In

type Except[-A] = [E, Ans] =>> Except.Syn[A, E, Ans]

object Except:

  trait Syn[-A, E, Ans]:
    def raise: Op[A, Nothing, E, Ans]

  def apply[A]: Ops[A] = new Ops[A]

  private[evidence] final class Ops[A](val dummy: Boolean = true) extends AnyVal:
    def raise[E](a: A)(using In[Except[A], E]): Eff[E, Nothing] =
      Eff.perform[A, Nothing, E, Except[A]](
        [E, Ans] => (_: Except[A][E, Ans]).raise
      )(a)

    def handleError[E, Ans](
        f: A => Ans
    ): Eff[Except[A] :* E, Ans] => Eff[E, Ans] = handleErrorWith(f(_).pure)

    def recoverWith[E, Ans](
        pf: PartialFunction[A, Eff[E, Ans]]
    ): Eff[Except[A] :* E, Ans] => Except[A] :? E ?=> Eff[E, Ans] =
      handleErrorWith(a => pf.applyOrElse(a, Except[A].raise))(_)

    def recover[E, Ans](
        pf: PartialFunction[A, Ans]
    ): Eff[Except[A] :* E, Ans] => Except[A] :? E ?=> Eff[E, Ans] = recoverWith(
      pf(_).pure
    )

    def toOption[E, Ans]: Eff[Except[A] :* E, Ans] => Eff[E, Option[Ans]] =
      Eff.handlerRet(
        (b: Ans) => b.some,
        new Syn[A, E, Option[Ans]] {
          val raise = Op.except[A, E, Option[Ans]](_ => None.pure)
        },
        _
      )

    def toEither[E, Ans]: Eff[Except[A] :* E, Ans] => Eff[E, Either[A, Ans]] =
      Eff.handlerRet(
        (b: Ans) => Right(b),
        new Syn[A, E, Either[A, Ans]] {
          val raise = Op.except[A, E, Either[A, Ans]](Left(_).pure)
        },
        _
      )

    def handleErrorWith[A, E, Ans](
        f: A => Eff[E, Ans]
    ): Eff[Except[A] :* E, Ans] => Eff[E, Ans] =
      Eff.handler(
        new Syn[A, E, Ans]:
          def raise = Op.except[A, E, Ans](f(_))
        ,
        _
      )


extension [A, E, Ans](eff: Eff[Except[A] :* E, Ans])
  def toEither: Eff[E, Either[A, Ans]] = Except[A].toEither(eff)
