package net.mentalarray.chat

import cats.effect.Sync
import cats.implicits._
import org.jline.reader.LineReaderBuilder
import org.jline.reader.UserInterruptException
import org.jline.utils.{AttributedString, AttributedStringBuilder, AttributedStyle}

import scala.annotation.tailrec

/**
  * A rich [[Console]]] capable of displaying colors and effects.
  */
final class RichConsole[F[_]] private (appName: String)(implicit F: Sync[F]) extends Console[F] {

  private[this] val reader = LineReaderBuilder
    .builder()
    .appName(appName)
    .option(org.jline.reader.LineReader.Option.ERASE_LINE_ON_FINISH, true)
    .build()

  private val lineSeparator = System.lineSeparator()

  override def newline(): F[Unit] = Sync[F].delay(reader.printAbove(lineSeparator))

  override def putText(richText: RichText): F[Unit] = Sync[F].delay {
    richText.instructions match {
      case RichText.Text(text) :: Nil => reader.printAbove(text)
      case instructions               => reader.printAbove(buildAttributedString(instructions, false))
    }
  }

  override def putTextLn(richText: RichText): F[Unit] = Sync[F].delay {
    richText.instructions match {
      case RichText.Text(text) :: Nil => reader.printAbove(text + lineSeparator)
      case instructions               => reader.printAbove(buildAttributedString(instructions, true))
    }
  }

  override def putError(richText: RichText): F[Unit] = Sync[F].delay {
    val errorMsg = richText.instructions match {
      case RichText.Text(text) :: Nil => new AttributedString(text + lineSeparator)
      case instructions               => buildAttributedString(instructions, true)
    }

    reader.printAbove(
      new AttributedStringBuilder()
        .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
        .append(errorMsg)
        .toAttributedString
    )
  }

  override def readText: F[String] = Sync[F].delay(reader.readLine()).handleErrorWith {
    case _: UserInterruptException => Sync[F].pure("")
    case t                         => Sync[F].raiseError(t)
  }

  override def readText(prompt: String) = Sync[F].delay(reader.readLine(prompt)).handleErrorWith {
    case _: UserInterruptException => Sync[F].pure("")
    case t                         => Sync[F].raiseError(t)
  }

  private def buildAttributedString(
    richInstructions: Seq[RichText.Instruction],
    newline: Boolean = true
  ): AttributedString = {
    import RichText._

    @inline def colorCode(color: Color): Int = color match {
      case NamedColor(name)   => Colors.fromName(name)
      case IndexedColor(code) => code
      case HexColor(hex)      => Colors.fromHex(hex)
    }

    @inline def popStack(stack: List[Int]): (Option[Int], List[Int]) =
      if (stack.isEmpty) (None, stack)
      else
        stack.tail match {
          case head :: tl => (Some(head), head :: tl)
          case Nil        => (None, List.empty)
        }

    @inline def resetForeground(asb: AttributedStringBuilder) =
      asb.style((x: AttributedStyle) => x.foregroundDefault())

    @inline def resetBackground(asb: AttributedStringBuilder) =
      asb.style((x: AttributedStyle) => x.backgroundDefault())

    @tailrec def loop_rec(
      instructions: Seq[RichText.Instruction],
      acc: AttributedStringBuilder,
      foreStack: List[Int],
      backStack: List[Int]
    ): AttributedString = instructions match {
      case Text(text) :: tl => loop_rec(tl, acc.append(text), foreStack, backStack)

      case StartAttribute(attr) :: tl =>
        attr match {
          case Bold              => loop_rec(tl, acc.style(x => x.bold()), foreStack, backStack)
          case Underline         => loop_rec(tl, acc.style(x => x.underline()), foreStack, backStack)
          case Blink             => loop_rec(tl, acc.style(x => x.blink()), foreStack, backStack)
          case Reverse           => loop_rec(tl, acc.style(x => x.inverse()), foreStack, backStack)
          case Background(color) =>
            val code = colorCode(color)
            loop_rec(tl, acc.style(x => x.background(code)), foreStack, code :: backStack)

          case Foreground(color) =>
            val code = colorCode(color)
            loop_rec(tl, acc.style(x => x.foreground(code)), code :: foreStack, backStack)

          case _ => loop_rec(tl, acc, foreStack, backStack)
        }

      case ResetAttributes :: tl => loop_rec(tl, acc.style(AttributedStyle.DEFAULT), foreStack, backStack)

      case StopAttribute(attr) :: tl =>
        attr match {
          case Bold       => loop_rec(tl, acc.style(x => x.boldOff()), foreStack, backStack)
          case Underline  => loop_rec(tl, acc.style(x => x.underlineOff()), foreStack, backStack)
          case Blink      => loop_rec(tl, acc.style(x => x.blinkOff()), foreStack, backStack)
          case Reverse    => loop_rec(tl, acc.style(x => x.inverseOff()), foreStack, backStack)
          case Background =>
            popStack(backStack) match {
              case (Some(prev), popped) => loop_rec(tl, acc.style(x => x.background(prev)), foreStack, popped)
              case (None, backStack)    => loop_rec(tl, resetBackground(acc), foreStack, backStack)
            }

          case Foreground =>
            popStack(foreStack) match {
              case (Some(prev), popped) => loop_rec(tl, acc.style(x => x.foreground(prev)), popped, backStack)
              case (None, foreStack)    => loop_rec(tl, resetForeground(acc), foreStack, backStack)
            }

          case _ => loop_rec(tl, acc, foreStack, backStack)
        }

      case Nil =>
        if (newline) acc.ansiAppend(lineSeparator).toAttributedString
        else acc.toAttributedString
    }

    loop_rec(richInstructions.toList, new AttributedStringBuilder(), List.empty, List.empty)
  }

}

object RichConsole {

  def apply[F[_]: Sync](appName: String): F[RichConsole[F]] = Sync[F].delay(new RichConsole[F](appName))

}
