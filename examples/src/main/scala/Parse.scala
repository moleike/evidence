package examples

import evidence.*
import cats.implicits._
import evidence.effect.NonDet.given
import cats.Monad
import cats.MonoidK
import cats.Foldable
import cats.Alternative

type Parser[A] = PartialFunction[String, (A, String)]

trait Parse[E, Ans]:
  def satisfy[A]: Op[Parser[A], A, E, Ans]

object Parse:
  def satisfy[E, A](p: Parser[A]): Parse :? E ?=> Eff[E, A] =
    Eff.perform[Parser[A], A, E, Parse](
      [EE, Ans] => (_: Parse[EE, Ans]).satisfy[A]
    )(p)

  def many[E, A](p: Eff[E, A]): NonDet :? E ?=> Eff[E, List[A]] =
    Alternative[Eff[E, *]].combineK(many1(p), Nil.pure)

  def many1[E, A](p: Eff[E, A]): NonDet :? E ?=> Eff[E, List[A]] =
    for
      x  <- p
      xs <- many(p)
    yield x :: xs

  def parse[E, A](
      input: String
  ): NonDet :? E ?=> Eff[Parse :* E, A] => Eff[E, (A, String)] =
    Eff.handlerLocalRet(
      input,
      (x: A) => s => (x, s),
      new Parse:
        def satisfy[AA]: Op[Parser[AA], AA, State[String] :* E, (A, String)] =
          Op((p, k) =>
            for
              input <- State[String].get
              r <- p.unapply(input) match
                case Some((x, rest)) => State[String].put(rest) *> k(x)
                case _               => NonDet.empty[State[String] :* E]
            yield r
          )
      ,
      _
    )

  def symbol[E](c: Char): Parse :? E ?=> Eff[E, Char] = satisfy {
    case s if s.nonEmpty && s.charAt(0) == c => (c, s.tail)
  }

  def string[E](t: String): Parse :? E ?=> Eff[E, String] = satisfy {
    case s if s.startsWith(t) => (t, s.stripPrefix(t))
  }

  def digit[E]: Parse :? E ?=> Eff[E, Int] = satisfy {
    case s if s.nonEmpty && s.charAt(0).isDigit => (s.charAt(0).asDigit, s.tail)
  }

  def number[E]: (Parse :? E, NonDet :? E) ?=> Eff[E, Int] =
    many1(digit).map(_.foldLeft(0)(_ * 10 + _))

  def between[E, A, B, C](
      `open`: Eff[E, B],
      close: Eff[E, C]
  ): Parse :? E ?=> Eff[E, A] => Eff[E, A] = `open` *> _ <* close
