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
      ret: Ans => A,
      h: H[E, A],
      action: Eff[H :* E, Ans]
  ): Eff[E, A] =
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

  def handlerHideRetEff[E, Ans, H[_, _], H0[_, _], A](
      ret: Ans => Eff[H0 :* E, A],
      h: H[H0 :* E, A],
      action: Eff[H :* E, Ans]
  ): Eff[H0 :* E, A] =
    case ctx0 @ Ctx.CCons(m0, h0, f, cs) =>
      Ctl.prompt(m =>
        for
          x <- Ctl.under(
            Ctx.CCons(
              m,
              h,
              (c: Ctx[E]) => Ctx.CCons(m0, h0, f, c),
              cs
            ),
            action
          )
          r <- Ctl.under(ctx0, ret(x))
        yield r
      )

  def handlerLocal[E, Ans, H[_, _], A](
      init: A,
      h: H[State[A] :* E, Ans],
      action: Eff[H :* E, Ans]
  ): Eff[E, Ans] = State[A, E, Ans](init)(handlerHide(h, action)).map(_._1)

  def handlerLocalRet[E, Ans, H[_, _], A, B](
      init: A,
      ret: Ans => A => B,
      h: H[State[A] :* E, B],
      action: Eff[H :* E, Ans]
  ): Eff[E, B] = State[A, E, B](init)(
    handlerHideRetEff[E, Ans, H, State[A], B](
      (a: Ans) => State[A].get.map(ret(a)),
      h,
      action
    )
  )
    .map(_._1)

  extension [A](eff: Eff[Nothing, A])
    def run: A = eff(Ctx.CNil) match
      case Ctl.Pure(x) => x
      case Ctl.Yield(_, _, _) =>
        throw new RuntimeException("Unhandled operation") // should never happen
