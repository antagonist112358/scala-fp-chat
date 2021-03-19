package net.mentalarray.chat

import java.io.{BufferedReader, PrintStream}
import cats._
import cats.arrow.FunctionK
import cats.effect.Sync
import cats.syntax.show._

/**
  * Effect type agnostic `Console` with common methods to write and read from the standard console.
  *
  * For an implementation that uses standard input/output,
  * see
  *
  * Use it either in a DSL style:
  *
  * {{{
  *   import cats.effect.IO
  *
  *   val c = Console.stdio[IO]
  *
  *   val program: IO[Unit] =
  *     for {
  *       _ <- c.putText("Please enter your name: ")
  *       n <- c.readText
  *       _ <- if (n.nonEmpty) c.putTextLn(s"Hello $$n!")
  *            else c.putError("Name is empty!")
  *     } yield ()
  * }}}
  *
  * Or in Tagless Final encoding:
  *
  * {{{
  *   import cats.Monad
  *   import cats.effect._
  *
  *   def myProgram[F[_]: Monad](implicit C: Console[F]): F[Unit] =
  *     for {
  *       _ <- C.putTextLn("Please enter your name: ")
  *       n <- C.readText
  *       _ <- if (n.nonEmpty) C.putTextLn(s"Hello $$n!")
  *            else C.putError("Name is empty!")
  *     } yield ()
  * }}}
  */
trait Console[F[_]] { self =>
  def newline(): F[Unit]

  def putText(text: String): F[Unit] = putText(RichText.raw(text))
  def putText(richText: RichText): F[Unit]

  def putTextLn(text: String): F[Unit] = putTextLn(RichText.raw(text))
  def putTextLn(richText: RichText): F[Unit]

  def readText: F[String]
  def readText(prompt: String): F[String]

  def putError(msg: String): F[Unit] = putError(RichText.raw(msg))
  def putError(richText: RichText): F[Unit]

  final def mapK[G[_]](nat: F ~> G): Console[G] = WrappedConsole[G, F](self, nat)

}

object Console {

  def apply[F[_]](implicit F: Console[F]): Console[F] = F

  /**
    * Constructs a console that prints to standard output, prints errors to standard error output, and reads from standard input.
    *
    * For a variant that allows customization of input/output streams, see [[basic]].
    */
  def stdio[F[_]: Sync]: F[Console[F]] = Sync[F].defer(basic(scala.Console.out, scala.Console.err, scala.Console.in))

  /**
    * Constructs a basic (no formatting) console that prints to the given `out` and `err` streams, and reads from the `in` reader.
    *
    * For a variant that defaults to standard input/output, see [[stdio]].
    */
  def basic[F[_]: Sync](out: PrintStream, err: PrintStream, in: BufferedReader): F[Console[F]] = Sync[F].delay(new BasicConsole[F](out, err, in))

}

private final class BasicConsole[F[_]: Sync](
  private val out: PrintStream,
  private val err: PrintStream,
  private val in: BufferedReader
) extends Console[F] {
  private val F = implicitly[Sync[F]]

  override def newline(): F[Unit] = F.delay(out.println())

  override def putText(richText: RichText): F[Unit] = F.delay {
    richText.instructions
      .collect { case RichText.Text(text) => text }
      .foreach(out.print)
  }

  override def putTextLn(richText: RichText): F[Unit] = F.delay {
    richText.instructions
      .collect { case RichText.Text(text) => text }
      .foreach(out.print)
    out.println()
  }

  override def readText: F[String] = F.delay(in.readLine())

  override def readText(prompt: String) = F.delay {
    out.print(prompt)
    in.readLine()
  }

  override def putError(richText: RichText): F[Unit] = F.delay {
    richText.instructions
      .collect { case RichText.Text(text) => text }
      .foreach(err.print)
    err.println()
  }

}

private final case class WrappedConsole[G[_], F[_]](
  underlying: Console[F],
  functionK: FunctionK[F, G]
) extends Console[G] {
  def newline(): G[Unit]                     = functionK(underlying.newline())
  def putText(richText: RichText): G[Unit]   = functionK(underlying.putText(richText))
  def putTextLn(richText: RichText): G[Unit] = functionK(underlying.putTextLn(richText))
  def readText: G[String]                    = functionK(underlying.readText)
  def readText(prompt: String): G[String]    = functionK(underlying.readText(prompt))
  def putError(richText: RichText): G[Unit]  = functionK(underlying.putError(richText))
}
