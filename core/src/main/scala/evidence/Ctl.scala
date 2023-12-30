package evidence

import cats.Monad

//control monad for multi-prompt delimited continuations
enum Ctl[+A]:

  case Pure(result: A)

  case Yield[A, B, Ans](
      marker: Marker[Ans],
      op: (B => Ctl[Ans]) => Ctl[Ans],
      cont: B => Ctl[A]
  ) extends Ctl[A]

  def lift[E]: Eff[E, A] = _ => this

object Ctl:

  given Monad[Ctl] with {
    def pure[A](x: A): Ctl[A] = Pure(x)

    def flatMap[A, B](ca: Ctl[A])(f: A => Ctl[B]): Ctl[B] =
      ca match
        case Pure(x)            => f(x)
        case Yield(m, op, cont) => Yield(m, op, kcompose(f, cont))

    def tailRecM[A, B](x: A)(f: A => Ctl[Either[A, B]]): Ctl[B] =
      flatMap(f(x)) {
        case Right(x)    => pure(x)
        case Left(nextA) => tailRecM(nextA)(f)
      }
  }

  def kcompose[A, B, C](g: B => Ctl[C], f: A => Ctl[B])(x: A): Ctl[C] =
    f(x) match
      case Ctl.Pure(x)            => g(x)
      case Ctl.Yield(m, op, cont) => Ctl.Yield(m, op, kcompose(g, cont))

  def under[E, A](ctx: Ctx[E], eff: Eff[E, A]): Ctl[A] = eff(ctx)

  def run[A](ctl: Ctl[A]): A = ctl match
    case Pure(x)        => x
    case Yield(_, _, _) => throw new RuntimeException("Unhandled operation")

  def mprompt[A](m: Marker[A], ctl: Ctl[A]): Ctl[A] = ctl match
    case Pure(a) => Pure(a)
    case Yield(n, op, cont): Yield[A, _, A] if m == n =>
      op(x => mprompt(m, cont(x)))
    case Yield(n, op, cont) => Yield(n, op, x => mprompt(m, cont(x)))

  def prompt[A](action: Marker[A] => Ctl[A]): Ctl[A] =
    Marker.fresh(m => mprompt(m, action(m)))
