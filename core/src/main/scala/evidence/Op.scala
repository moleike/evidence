package evidence

import cats.implicits._

import Ctl.*

trait Op[-A, +B, E, Ans] extends ((Marker[Ans], Ctx[E], A) => Ctl[B])

object Op:

  private def yield_[B, E, Ans](
      m: Marker[Ans],
      ctx: Ctx[E],
      f: (B => Ctl[Ans]) => Eff[E, Ans]
  ): Ctl[B] =
    Yield(m, (k: B => Ctl[Ans]) => under(ctx, f(k)), Pure(_))

  // general operation with resumptions
  def apply[A, B, E, Ans](
      f: (A, B => Eff[E, Ans]) => Eff[E, Ans]
  ): Op[A, B, E, Ans] = (m, ctx, x) =>
    yield_(m, ctx, (k: B => Ctl[Ans]) => f(x, k(_).lift))

  // resume once, more efficient version of:
  // Op((x, k) => f(x).flatMap(k))
  def function[A, B, E, Ans](f: A => Eff[E, B]): Op[A, B, E, Ans] =
    (_, ctx, x) => under(ctx, f(x))

  // resume with a constant value, same as:
  // Op((_, k) => k(x))
  def value[A, E, Ans](x: A): Op[Unit, A, E, Ans] = function(_ => x.pure)

  // create an operation that never resumes (an exception).
  def except[A, E, Ans](f: A => Eff[E, Ans]): Op[A, Nothing, E, Ans] =
    (m, ctx, x) => yield_(m, ctx, _ => f(x))
