package evidence

import cats.implicits._

trait Op[-A, +B, E, Ans] extends ((Marker[Ans], Ctx[E], A) => Ctl[B])

object Op:
  // general operation with resumptions
  def apply[A, B, E, Ans](
      f: (A, B => Eff[E, Ans]) => Eff[E, Ans]
  ): Op[A, B, E, Ans] = (m, ctx, x) =>
    `yield`(m, ctx, (k: B => Ctl[Ans]) => f(x, k(_).lift))

  // resume once, more efficient version of:
  // Op((x, k) => f(x).flatMap(k))
  def function[A, B, E, Ans](f: A => Eff[E, B]): Op[A, B, E, Ans] =
    (_, ctx, x) => Ctl.under(ctx, f(x))

  // resume with a constant value, same as:
  // Op((_, k) => k(x))
  def value[A, E, Ans](x: A): Op[Unit, A, E, Ans] = function(_ => x.pure)

  // create an operation that never resumes (an exception).
  def except[A, E, Ans](f: A => Eff[E, Ans]): Op[A, Nothing, E, Ans] =
    (m, ctx, x) => `yield`(m, ctx, _ => f(x))

  private def `yield`[B, E, Ans](
      m: Marker[Ans],
      ctx: Ctx[E],
      f: (B => Ctl[Ans]) => Eff[E, Ans]
  ): Ctl[B] =
    Ctl.Yield(m, (k: B => Ctl[Ans]) => Ctl.under(ctx, f(k)), Ctl.Pure(_))
