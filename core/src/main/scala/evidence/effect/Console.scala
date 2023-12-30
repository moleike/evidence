package evidence
package effect

import cats.implicits._

type Console = [E, Ans] =>> Console.Syn[E, Ans]

object Console:
  trait Syn[E, Ans]:
    def println: Op[String, Unit, E, Ans]
    def readLine: Op[Unit, String, E, Ans]

  def println[E](msg: String): Console :? E ?=> Eff[E, Unit] =
    Eff.perform[String, Unit, E, Console](
      [EE, Ans] => (_: Console[EE, Ans]).println
    )(msg)

  def readLine[E]: Console :? E ?=> Eff[E, String] =
    Eff.perform[Unit, String, E, Console](
      [EE, Ans] => (_: Console[EE, Ans]).readLine
    )(())

  def console[E, Ans]: Eff[Console :* E, Ans] => Eff[E, Ans] =
    Eff.handler(
      new Syn[E, Ans]:
        val println =
          Op.function[String, Unit, E, Ans](System.out.println(_).pure)
        val readLine =
          Op.function[Unit, String, E, Ans](_ => scala.io.StdIn.readLine().pure)
      ,
      _
    )

// runs an Eff compuation with default Console handler
extension [A](eff: Eff[Console :* Nothing, A])
  def runC: A = Console.console(eff).run
