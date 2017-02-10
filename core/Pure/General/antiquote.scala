/*  Title:      Pure/General/antiquote.scala
    Author:     Makarius

Antiquotations within plain text.
*/

package isabelle


import scala.util.parsing.input.CharSequenceReader


object Antiquote
{
  sealed abstract class Antiquote { def source: String }
  case class Text(source: String) extends Antiquote
  case class Control(source: String) extends Antiquote
  case class Antiq(source: String) extends Antiquote


  /* parsers */

  object Parsers extends Parsers

  trait Parsers extends Scan.Parsers
  {
    private val txt: Parser[String] =
      rep1(many1(s => !Symbol.is_control(s) && !Symbol.is_open(s) && s != "@") |
        "@" <~ guard(opt_term(one(s => s != "{")))) ^^ (x => x.mkString)

    val control: Parser[String] =
      opt(one(Symbol.is_control)) ~ cartouche ^^ { case Some(x) ~ y => x + y case None ~ x => x } |
      one(Symbol.is_control)

    val antiq_other: Parser[String] =
      many1(s => s != "\"" && s != "`" && s != "}" && !Symbol.is_open(s) && !Symbol.is_close(s))

    private val antiq_body: Parser[String] =
      quoted("\"") | (quoted("`") | (cartouche | antiq_other))

    val antiq: Parser[String] =
      "@{" ~ rep(antiq_body) ~ "}" ^^ { case x ~ y ~ z => x + y.mkString + z }

    val antiquote: Parser[Antiquote] =
      txt ^^ (x => Text(x)) | (control ^^ (x => Control(x)) | antiq ^^ (x => Antiq(x)))
  }


  /* read */

  def read(input: CharSequence): List[Antiquote] =
  {
    Parsers.parseAll(Parsers.rep(Parsers.antiquote), new CharSequenceReader(input)) match {
      case Parsers.Success(xs, _) => xs
      case Parsers.NoSuccess(_, next) =>
        error("Malformed quotation/antiquotation source" +
          Position.here(Position.Line(next.pos.line)))
    }
  }
}

