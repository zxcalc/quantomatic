/*  Title:      Pure/Isar/token.scala
    Author:     Makarius

Outer token syntax for Isabelle/Isar.
*/

package isabelle


object Token
{
  /* tokens */

  object Kind extends Enumeration
  {
    val COMMAND = Value("command")
    val KEYWORD = Value("keyword")
    val IDENT = Value("identifier")
    val LONG_IDENT = Value("long identifier")
    val SYM_IDENT = Value("symbolic identifier")
    val VAR = Value("schematic variable")
    val TYPE_IDENT = Value("type variable")
    val TYPE_VAR = Value("schematic type variable")
    val NAT = Value("natural number")
    val FLOAT = Value("floating-point number")
    val STRING = Value("string")
    val ALT_STRING = Value("back-quoted string")
    val VERBATIM = Value("verbatim text")
    val CARTOUCHE = Value("text cartouche")
    val SPACE = Value("white space")
    val COMMENT = Value("comment text")
    val ERROR = Value("bad input")
    val UNPARSED = Value("unparsed input")
  }


  /* parsers */

  object Parsers extends Parsers

  trait Parsers extends Scan.Parsers
  {
    private def delimited_token: Parser[Token] =
    {
      val string = quoted("\"") ^^ (x => Token(Token.Kind.STRING, x))
      val alt_string = quoted("`") ^^ (x => Token(Token.Kind.ALT_STRING, x))
      val verb = verbatim ^^ (x => Token(Token.Kind.VERBATIM, x))
      val cart = cartouche ^^ (x => Token(Token.Kind.CARTOUCHE, x))
      val cmt = comment ^^ (x => Token(Token.Kind.COMMENT, x))

      string | (alt_string | (verb | (cart | cmt)))
    }

    private def other_token(lexicon: Scan.Lexicon, is_command: String => Boolean)
      : Parser[Token] =
    {
      val letdigs1 = many1(Symbol.is_letdig)
      val sub = one(s => s == Symbol.sub_decoded || s == "\\<^sub>")
      val id =
        one(Symbol.is_letter) ~
          (rep(letdigs1 | (sub ~ letdigs1 ^^ { case x ~ y => x + y })) ^^ (_.mkString)) ^^
        { case x ~ y => x + y }

      val nat = many1(Symbol.is_digit)
      val natdot = nat ~ "." ~ nat ^^ { case x ~ y ~ z => x + y + z }
      val id_nat = id ~ opt("." ~ nat) ^^ { case x ~ Some(y ~ z) => x + y + z case x ~ None => x }

      val ident = id ~ rep("." ~> id) ^^
        { case x ~ Nil => Token(Token.Kind.IDENT, x)
          case x ~ ys => Token(Token.Kind.LONG_IDENT, (x :: ys).mkString(".")) }

      val var_ = "?" ~ id_nat ^^ { case x ~ y => Token(Token.Kind.VAR, x + y) }
      val type_ident = "'" ~ id ^^ { case x ~ y => Token(Token.Kind.TYPE_IDENT, x + y) }
      val type_var = "?'" ~ id_nat ^^ { case x ~ y => Token(Token.Kind.TYPE_VAR, x + y) }
      val nat_ = nat ^^ (x => Token(Token.Kind.NAT, x))
      val float =
        ("-" ~ natdot ^^ { case x ~ y => x + y } | natdot) ^^ (x => Token(Token.Kind.FLOAT, x))

      val sym_ident =
        (many1(Symbol.is_symbolic_char) | one(sym => Symbol.is_symbolic(sym))) ^^
        (x => Token(Token.Kind.SYM_IDENT, x))

      val command_keyword =
        literal(lexicon) ^^
          (x => Token(if (is_command(x)) Token.Kind.COMMAND else Token.Kind.KEYWORD, x))

      val space = many1(Symbol.is_blank) ^^ (x => Token(Token.Kind.SPACE, x))

      val recover_delimited =
        (recover_quoted("\"") |
          (recover_quoted("`") |
            (recover_verbatim |
              (recover_cartouche | recover_comment)))) ^^ (x => Token(Token.Kind.ERROR, x))

      val bad = one(_ => true) ^^ (x => Token(Token.Kind.ERROR, x))

      space | (recover_delimited |
        (((ident | (var_ | (type_ident | (type_var | (float | (nat_ | sym_ident)))))) |||
          command_keyword) | bad))
    }

    def token(lexicon: Scan.Lexicon, is_command: String => Boolean): Parser[Token] =
      delimited_token | other_token(lexicon, is_command)

    def token_line(lexicon: Scan.Lexicon, is_command: String => Boolean, ctxt: Scan.Line_Context)
      : Parser[(Token, Scan.Line_Context)] =
    {
      val string =
        quoted_line("\"", ctxt) ^^ { case (x, c) => (Token(Token.Kind.STRING, x), c) }
      val alt_string =
        quoted_line("`", ctxt) ^^ { case (x, c) => (Token(Token.Kind.ALT_STRING, x), c) }
      val verb = verbatim_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.VERBATIM, x), c) }
      val cart = cartouche_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.CARTOUCHE, x), c) }
      val cmt = comment_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.COMMENT, x), c) }
      val other = other_token(lexicon, is_command) ^^ { case x => (x, Scan.Finished) }

      string | (alt_string | (verb | (cart | (cmt | other))))
    }
  }


  /* token reader */

  object Pos
  {
    val none: Pos = new Pos(0, "")
  }

  final class Pos private[Token](val line: Int, val file: String)
    extends scala.util.parsing.input.Position
  {
    def column = 0
    def lineContents = ""

    def advance(token: Token): Pos =
    {
      var n = 0
      for (c <- token.content if c == '\n') n += 1
      if (n == 0) this else new Pos(line + n, file)
    }

    def position: Position.T = Position.Line_File(line, file)
    override def toString: String = Position.here_undelimited(position)
  }

  abstract class Reader extends scala.util.parsing.input.Reader[Token]

  private class Token_Reader(tokens: List[Token], val pos: Pos) extends Reader
  {
    def first = tokens.head
    def rest = new Token_Reader(tokens.tail, pos.advance(first))
    def atEnd = tokens.isEmpty
  }

  def reader(tokens: List[Token], file: String = ""): Reader =
    new Token_Reader(tokens, new Pos(1, file))
}


sealed case class Token(val kind: Token.Kind.Value, val source: String)
{
  def is_command: Boolean = kind == Token.Kind.COMMAND
  def is_keyword: Boolean = kind == Token.Kind.KEYWORD
  def is_delimiter: Boolean = is_keyword && !Symbol.is_ascii_identifier(source)
  def is_ident: Boolean = kind == Token.Kind.IDENT
  def is_sym_ident: Boolean = kind == Token.Kind.SYM_IDENT
  def is_string: Boolean = kind == Token.Kind.STRING
  def is_nat: Boolean = kind == Token.Kind.NAT
  def is_float: Boolean = kind == Token.Kind.FLOAT
  def is_name: Boolean =
    kind == Token.Kind.IDENT ||
    kind == Token.Kind.SYM_IDENT ||
    kind == Token.Kind.STRING ||
    kind == Token.Kind.NAT
  def is_xname: Boolean = is_name || kind == Token.Kind.LONG_IDENT
  def is_text: Boolean = is_xname || kind == Token.Kind.VERBATIM || kind == Token.Kind.CARTOUCHE
  def is_space: Boolean = kind == Token.Kind.SPACE
  def is_comment: Boolean = kind == Token.Kind.COMMENT
  def is_improper: Boolean = is_space || is_comment
  def is_proper: Boolean = !is_space && !is_comment
  def is_error: Boolean = kind == Token.Kind.ERROR
  def is_unparsed: Boolean = kind == Token.Kind.UNPARSED

  def is_unfinished: Boolean = is_error &&
   (source.startsWith("\"") ||
    source.startsWith("`") ||
    source.startsWith("{*") ||
    source.startsWith("(*") ||
    source.startsWith(Symbol.open) ||
    source.startsWith(Symbol.open_decoded))

  def is_begin: Boolean = is_keyword && source == "begin"
  def is_end: Boolean = is_keyword && source == "end"

  def content: String =
    if (kind == Token.Kind.STRING) Scan.Parsers.quoted_content("\"", source)
    else if (kind == Token.Kind.ALT_STRING) Scan.Parsers.quoted_content("`", source)
    else if (kind == Token.Kind.VERBATIM) Scan.Parsers.verbatim_content(source)
    else if (kind == Token.Kind.CARTOUCHE) Scan.Parsers.cartouche_content(source)
    else if (kind == Token.Kind.COMMENT) Scan.Parsers.comment_content(source)
    else source

  def text: (String, String) =
    if (is_keyword && source == ";") ("terminator", "")
    else (kind.toString, source)
}

