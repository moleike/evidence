package examples

import evidence.*
import evidence.effect.*
import cats.implicits._
import evidence.effect.NonDet.given
import cats.Alternative

type Parse = [E, Ans] =>> Parse.Syn[E, Ans]

object Parse:

  trait Parser[A] extends (String => Option[(A, String)])

  trait Syn[E, Ans]:
    def satisfy[A]: Op[Parser[A], A, E, Ans]

  def satisfy[E, A](p: Parser[A]): Parse :? E ?=> Eff[E, A] =
    Eff.perform[Parser[A], A, E, Parse](
      [EE, Ans] => (_: Parse[EE, Ans]).satisfy[A]
    )(p)

  def many[E, A](p: Eff[E, A]): NonDet :? E ?=> Eff[E, List[A]] =
    Alternative[Eff[E, *]].combineK(many1(p), List.empty[A].pure)

  def many1[E, A](p: Eff[E, A]): NonDet :? E ?=> Eff[E, List[A]] =
    for
      x <- p
      xs <- many(p)
    yield x :: xs

  def parse[E, A](
      input: String
  ): NonDet :? E ?=> Eff[Parse :* E, A] => Eff[E, (A, String)] =
    Eff.handlerLocalRet(
      input,
      (x: A) => s => (x, s),
      new Syn[Local[String] :* E, (A, String)]:
        def satisfy[AA]: Op[Parser[AA], AA, Local[String] :* E, (A, String)] =
          Op((p, k) =>
            for
              input <- Local.get
              r <- p(input) match
                case Some((x, rest)) => Local.put(rest) *> k(x)
                case _               => NonDet.empty[Local[String] :* E]
            yield r
          )
      ,
      _
    )

  def symbol[E](c: Char): Parse :? E ?=> Eff[E, Char] = satisfy {
    case s if s.nonEmpty && s.charAt(0) == c => Some(c, s.tail)
    case _                                   => None
  }

  def digit[E]: Parse :? E ?=> Eff[E, Int] = satisfy {
    case s if s.nonEmpty && s.charAt(0).isDigit =>
      Some(s.charAt(0).asDigit, s.tail)
    case _ => None
  }

  def number[E]: (Parse :? E, NonDet :? E) ?=> Eff[E, Int] =
    many1(digit).map(_.foldLeft(0)(_ * 10 + _))
