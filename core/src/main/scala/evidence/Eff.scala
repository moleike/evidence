package evidence

import cats.Monad
import cats.syntax.all._

trait Eff[E, +A] extends (Ctx[E] => Ctl[A])

object Eff:
  given [E]: Monad[Eff[E, *]] with {
    def pure[A](x: A): Eff[E, A] = _ => x.pure[Ctl]

    def flatMap[A, B](ea: Eff[E, A])(f: A => Eff[E, B]): Eff[E, B] = ctx =>
      for
        ctl <- ea(ctx)
        res <- Ctl.under(ctx, f(ctl))
      yield res

    def tailRecM[A, B](x: A)(f: A => Eff[E, Either[A, B]]): Eff[E, B] =
      flatMap(f(x)) {
        case Right(x)    => pure(x)
        case Left(nextA) => tailRecM(nextA)(f)
      }
  }

  def perform[A, B, E, H[_, _]](
      selectOp: [EE, Ans] => H[EE, Ans] => Op[A, B, EE, Ans]
  )(x: A): H :? E ?=> Eff[E, B] = ctx =>
    summon[H :? E].subCtx(ctx).value match
      case Ctx.CCons(m, h, f, cs) => selectOp(h)(m, f(cs), x)

  def handler[E, Ans, H[_, _]](
      h: H[E, Ans],
      action: Eff[H :* E, Ans]
  ): Eff[E, Ans] = ctx =>
    Ctl.prompt(m => Ctl.under(Ctx.CCons(m, h, identity[Ctx[E]], ctx), action))

  def handlerRet[E, Ans, H[_, _], A](
      ret: A => Ans,
      h: H[E, Ans],
      action: Eff[H :* E, A]
  ): Eff[E, Ans] =
    handler(h, action.map(ret(_)))

  def handlerHide[E, Ans, H[_, _], H0[_, _]](
      h: H[H0 :* E, Ans],
      action: Eff[H :* E, Ans]
  ): Eff[H0 :* E, Ans] =
    case Ctx.CCons(m0, h0, f, cs) =>
      Ctl.prompt(m =>
        Ctl.under(
          Ctx.CCons(
            m,
            h,
            (c: Ctx[E]) => Ctx.CCons(m0, h0, f, c),
            cs
          ),
          action
        )
      )

  extension [A](eff: Eff[Nothing, A])
    def run: A = eff(Ctx.CNil) match
      case Ctl.Pure(x) => x
      case Ctl.Yield(_, _, _) =>
        throw new RuntimeException("Unhandled operation") // should never happen
