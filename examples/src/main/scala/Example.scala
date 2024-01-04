package examples

import evidence.*
import evidence.effect.*
import cats.implicits._
import cats.Monad
import cats.Monoid
import cats.MonoidK
import cats.Foldable
import cats.mtl.Ask
import cats.mtl.Raise
import cats.data.Chain
import cats.mtl.Tell

object Example:

  def main(args: Array[String]): Unit =
    def greet[E]: (Reader[Long] :? E, Reader[String] :? E) ?=> Eff[E, String] =
      for
        p <- Reader[String].ask
        a <- Reader[Long].ask
      yield s"Hello, $p, $a times!"

    def greet5Times[E]: Reader[String] :? E ?=> Eff[E, String] =
      Reader[Long].const(5L)(greet)

    println(Reader[String].const("Leiva")(greet5Times).run)

    def there[E]: Eff[Reader[String] :* E, String] =
      Reader[String].scope("there")(greet5Times)

    println(Reader[String].const("Leiva")(there).run)

    // using cats-mtl

    // import Reader.given

    // def greetMtl[F[_]: Monad](using ask: Ask[F, String]): F[String] =
    //  for p <- ask.ask
    //  yield s"Hello, $p"

    // println(Reader.const("mtl")(greetMtl).run)

    // exceptions

    object DivByZeroError extends Throwable

    def div[E](x: Long, y: Long): Except[Throwable] :? E ?=> Eff[E, Long] =
      if y == 0 then Except[Throwable].raise(DivByZeroError)
      else (x / y).pure

    println(div(42, 0).toEither.run)

    def safeDiv[E](
        x: Long,
        y: Long
    ): Except[Throwable] :? E ?=> Eff[E, Long] =
      Except[Throwable].recover { case DivByZeroError =>
        0L
      }(div(x, y))

    println(safeDiv(42, 0).toEither.run)

    // using cats-mtl

    // import Except.given

    // def divMtl[F[_]: Applicative](x: Long, y: Long)(implicit
    //    F: Raise[F, Throwable]
    // ): F[Long] =
    //  if y == 0 then F.raise(new ArithmeticException)
    //  else (x / y).pure

    // def divEff[E](x: Long, y: Long): Except[Throwable] :? E ?=> Eff[E, Long] =
    //  divMtl[Eff[E, *]](x, y)

    def foo[E]: (
        Reader[String] :? E,
        Except[String] :? E,
        Console :? E,
        State[Int] :? E
    ) ?=> Eff[E, String] =
      for
        name <- Reader[String].ask
        str <-
          if name == "Joe" then "nice!".pure[Eff[E, *]]
          else Except[String].raise("ouch")
        _ <- Console.println(s"His name is $name")
      yield str

    println(
      (State
        .state(0)(
          Reader[String]
            .const("Joe")(Except[String].handleError(_ => "not Joe")(foo))
        ))
        .runC
    )

    implicitly[ArithmeticException <:< Throwable]
    implicitly[
      Except[Throwable][Unit, Unit] <:< Except[ArithmeticException][Unit, Unit]
    ]

    def invert[E]: State[Boolean] :? E ?=> Eff[E, Unit] =
      for
        a <- State[Boolean].get
        _ <- State[Boolean].put(!a)
      yield ()

    println(State.state(true)(invert).run)

    type Log = Chain[String]

    def ex[E]: Writer[Log] :? E ?=> Eff[E, Unit] =
      for
        _ <- Writer[Log].tell(Chain.one("foo"))
        _ <- Writer[Log].tell(Chain.one("bar"))
      yield ()

    println(ex.runW.run)

    def xor[E]: NonDet :? E ?=> Eff[E, Boolean] =
      for
        x <- NonDet.choose
        y <- NonDet.choose
      yield (x && !y) || (!x && y)

    println(NonDet.allResults[Nothing, Boolean, Vector](xor).run)

    def branches[F[_]: Monad](using
        F: MonoidK[F],
        G: Foldable[List]
    ): F[(Int, Int)] =
      for
        x <- G.foldK(List(1, 2, 3).map(_.pure[F]))
        y <- G.foldK(List(4, 5, 6).map(_.pure[F]))
      yield (x, y)

    println(NonDet.allResults[Nothing, (Int, Int), Vector](branches).run)

    // def logging[F[_]: Monad](implicit F: Tell[F, Log]): F[Unit] =
    //  // Example of some logging activity in your application
    //  for {
    //    _ <- F.tell(Chain.one("First log"))
    //    _ <- F.tell(Chain.one("Second log"))
    //  } yield ()

    def sendLogsToStdOut[A, E](
        logProgram: Eff[Writer[Log] :* E, A]
    ): (
        Console :? E,
        Writer[Log] :? E,
        Monoid[Log]
    ) ?=> Eff[E, A] =
      logProgram.listen
        .flatMap((a, logs) => logs.traverse_(Console.println(_)).as(a))

    def prependMessage[E, A](
        logProgram: Eff[Writer[Log] :* E, A]
    ): Writer[Log] :? E ?=> Eff[E, A] =
      logProgram.censor((log: Log) => log.prepend("Hello"))

    // println(
    //  Writer
    //    .writer[Console :* Nothing, Log, Unit](
    //      sendLogsToStdOut(prependMessage(logging))
    //    )
    //    .runC
    // )

    def ex2[E]: Chronicle[String] :? E ?=> Eff[E, Int] =
      for
        _ <- Chronicle[String].dictate("foo")
        _ <- Chronicle[String].dictate("bar")
      // _ <- Chronicle[String].confess("ohoh")
      yield 42

    println(Chronicle.materialize[Nothing, String, Int](ex2).run)
