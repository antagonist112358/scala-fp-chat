package net.mentalarray.chat

import fastparse._
import NoWhitespace._
import shapeless.ops.nat

sealed abstract class RichText {
  import RichText.RichTextImpl

  def instructions: Seq[RichText.Instruction]

  def +(other: RichText): RichText = RichTextImpl(this.toString + other.toString, instructions ++ other.instructions)

}

object RichText {

  final def raw(string: String): RichText = RichTextImpl(string, Text(string) :: Nil)

  final def formatted(input: String): RichText = parse(input, Parser(_)) match {
    case Parsed.Success(rt, _) => RichTextImpl(input, rt)
    case fail: Parsed.Failure  =>
      println(s"Parsing failure: $fail")
      RichTextImpl(input, Text(input) :: Nil)
  }

  /**
    * RichText string interpolator.
    *
    * @example
    * {{{
    *   val color = "blue"
    *   val greet = "Hello"
    *   val richText = r"[b][fg:color]greet[/b], World!"
    * }}}
    *
    * @note Format Options:
    *   [b]        Start using bold text
    *   [u]        Start underlining text
    *   [bl]       Start blinking text
    *   [r]        Start reversing foreground and background
    *   [fg:color] Start coloring the foreground with a named color (e.g. "red")
    *   [fg:id]    Same with an xterm 256-color code (0 - 256)
    *   [fg:hex]   Same with an arbitrary RGB hexadecimal color code
    *   [bg:color] Start coloring the background with a named color (e.g. "red")
    *   [bg:id]    Same with an xterm 256-color code (0 - 256)
    *   [bg:hex]   Same with an arbitrary RGB hexadecimal color code
    *
    *   [/b]       Stop using bold text
    *   [/u]       Stop underlining text
    *   [/bl]      Stop blinking text
    *   [/r]       Stop reversing foreground and background
    *   [/fg]      Stop coloring the foreground
    *   [/bg]      Stop coloring the background
    *   [/!]       Stop all
    */
  implicit class RichTextHelper(val sc: StringContext) extends AnyVal {

    def r(args: Any*): RichText = {
      val input = sc.s(args: _*)
      RichText.formatted(input)
    }

  }

  /* Instructions for `RichText` */

  sealed trait Instruction
  case class Text(text: String)                   extends Instruction
  case class StartAttribute(attribute: Attribute) extends Instruction
  case class StopAttribute(attribute: Attribute)  extends Instruction
  case object ResetAttributes                     extends Instruction

  sealed trait Attribute
  case object Bold                    extends Attribute
  case object Underline               extends Attribute
  case object Blink                   extends Attribute
  case object Reverse                 extends Attribute
  case object Foreground              extends Attribute
  case object Background              extends Attribute
  case class Foreground(color: Color) extends Attribute
  case class Background(color: Color) extends Attribute

  sealed trait Color
  case class NamedColor(name: String) extends Color
  case class IndexedColor(code: Int)  extends Color
  case class HexColor(hex: String)    extends Color

  private final case class RichTextImpl(formatting: String, instructions: Seq[Instruction]) extends RichText {
    override def toString = formatting
  }

  private final object Parser {

    def apply[_: P] = P((text | escape | block).rep).map(_.map(ins => ins: Instruction))

    private def letter[_: P]   = P(CharIn("a-z"))
    private def digit[_: P]    = P(CharIn("0-9"))
    private def hexDigit[_: P] = P(CharIn("0-9", "a-f", "A-F"))

    private def name[_: P]  = P(letter.rep(1).!)
    private def index[_: P] = P(digit.rep(1).!).map(_.toInt)

    private def hex[_: P] = P(
      ("#" ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit ~/ hexDigit).!
    )

    private def bold[_: P]       = P("b").map(_ => Bold)
    private def underline[_: P]  = P("u").map(_ => Underline)
    private def blink[_: P]      = P("bl").map(_ => Blink)
    private def reverse[_: P]    = P("r").map(_ => Reverse)
    private def foreground[_: P] = P("fg").map(_ => Foreground)
    private def background[_: P] = P("bg").map(_ => Background)

    private def attribute[_: P] = P(foreground | background | underline | blink | bold | reverse)

    private def namedColor[_: P]   = P(name).map(NamedColor)
    private def indexedColor[_: P] = P(index).map(IndexedColor)
    private def hexColor[_: P]     = P(hex).map(HexColor)

    private def color[_: P] = P(namedColor | indexedColor | hexColor)

    private def startAttribute[_: P] = P(attribute).map(StartAttribute)

    private def beginColor[_: P] = P(("fg" | "bg").! ~ ":" ~/ color).map {
      case ("fg", aColor) => StartAttribute(Foreground(aColor))
      case (_, aColor)    => StartAttribute(Background(aColor))
    }

    private def stop[_: P] = P("/" ~/ ("!".! | attribute)).map {
      case "!"             => ResetAttributes
      case attr: Attribute => StopAttribute(attr)
    }

    private def block[_: P]  = P("[" ~/ (beginColor | startAttribute | stop) ~/ "]")
    private def escape[_: P] = P("[[".!).map(_ => Text("["))
    private def text[_: P]   = P(CharsWhile(c => c != '[').!).map(Text)
  }

}
