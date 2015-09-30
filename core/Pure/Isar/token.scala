/*  Title:      Pure/Isar/token.scala
    Author:     Makarius

Outer token syntax for Isabelle/Isar.
*/

package isabelle


import scala.collection.mutable
import scala.util.parsing.input


object Token
{
  /* tokens */

  object Kind extends Enumeration
  {
    /*immediate source*/
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
    val SPACE = Value("white space")
    /*delimited content*/
    val STRING = Value("string")
    val ALT_STRING = Value("back-quoted string")
    val VERBATIM = Value("verbatim text")
    val CARTOUCHE = Value("text cartouche")
    val COMMENT = Value("comment text")
    /*special content*/
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

    private def other_token(keywords: Keyword.Keywords): Parser[Token] =
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

      val keyword =
        literal(keywords.minor) ^^ (x => Token(Token.Kind.KEYWORD, x)) |||
        literal(keywords.major) ^^ (x => Token(Token.Kind.COMMAND, x))

      val space = many1(Symbol.is_blank) ^^ (x => Token(Token.Kind.SPACE, x))

      val recover_delimited =
        (recover_quoted("\"") |
          (recover_quoted("`") |
            (recover_verbatim |
              (recover_cartouche | recover_comment)))) ^^ (x => Token(Token.Kind.ERROR, x))

      val bad = one(_ => true) ^^ (x => Token(Token.Kind.ERROR, x))

      space | (recover_delimited |
        (((ident | (var_ | (type_ident | (type_var | (float | (nat_ | sym_ident)))))) |||
          keyword) | bad))
    }

    def token(keywords: Keyword.Keywords): Parser[Token] =
      delimited_token | other_token(keywords)

    def token_line(keywords: Keyword.Keywords, ctxt: Scan.Line_Context)
      : Parser[(Token, Scan.Line_Context)] =
    {
      val string =
        quoted_line("\"", ctxt) ^^ { case (x, c) => (Token(Token.Kind.STRING, x), c) }
      val alt_string =
        quoted_line("`", ctxt) ^^ { case (x, c) => (Token(Token.Kind.ALT_STRING, x), c) }
      val verb = verbatim_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.VERBATIM, x), c) }
      val cart = cartouche_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.CARTOUCHE, x), c) }
      val cmt = comment_line(ctxt) ^^ { case (x, c) => (Token(Token.Kind.COMMENT, x), c) }
      val other = other_token(keywords) ^^ { case x => (x, Scan.Finished) }

      string | (alt_string | (verb | (cart | (cmt | other))))
    }
  }


  /* explode */

  def explode(keywords: Keyword.Keywords, inp: CharSequence): List[Token] =
  {
    val in: input.Reader[Char] = new input.CharSequenceReader(inp)
    Parsers.parseAll(Parsers.rep(Parsers.token(keywords)), in) match {
      case Parsers.Success(tokens, _) => tokens
      case _ => error("Unexpected failure of tokenizing input:\n" + inp.toString)
    }
  }

  def explode_line(keywords: Keyword.Keywords, inp: CharSequence, context: Scan.Line_Context)
    : (List[Token], Scan.Line_Context) =
  {
    var in: input.Reader[Char] = new input.CharSequenceReader(inp)
    val toks = new mutable.ListBuffer[Token]
    var ctxt = context
    while (!in.atEnd) {
      Parsers.parse(Parsers.token_line(keywords, ctxt), in) match {
        case Parsers.Success((x, c), rest) => toks += x; ctxt = c; in = rest
        case Parsers.NoSuccess(_, rest) =>
          error("Unexpected failure of tokenizing input:\n" + rest.source.toString)
      }
    }
    (toks.toList, ctxt)
  }


  /* implode */

  def implode(toks: List[Token]): String =
    toks match {
      case List(tok) => tok.source
      case _ => toks.map(_.source).mkString
    }


  /* token reader */

  object Pos
  {
    val none: Pos = new Pos(0, 0, "", "")
    val start: Pos = new Pos(1, 1, "", "")
    def file(file: String): Pos = new Pos(1, 1, file, "")
    def id(id: String): Pos = new Pos(0, 1, "", id)
    val command: Pos = id(Markup.COMMAND)
  }

  final class Pos private[Token](
      val line: Int,
      val offset: Symbol.Offset,
      val file: String,
      val id: String)
    extends scala.util.parsing.input.Position
  {
    def column = 0
    def lineContents = ""

    def advance(token: Token): Pos =
    {
      var line1 = line
      var offset1 = offset
      for (s <- Symbol.iterator(token.source)) {
        if (line1 > 0 && Symbol.is_newline(s)) line1 += 1
        if (offset1 > 0) offset1 += 1
      }
      if (line1 == line && offset1 == offset) this
      else new Pos(line1, offset1, file, id)
    }

    private def position(end_offset: Symbol.Offset): Position.T =
      (if (line > 0) Position.Line(line) else Nil) :::
      (if (offset > 0) Position.Offset(offset) else Nil) :::
      (if (end_offset > 0) Position.End_Offset(end_offset) else Nil) :::
      (if (file != "") Position.File(file) else Nil) :::
      (if (id != "") Position.Id_String(id) else Nil)

    def position(): Position.T = position(0)
    def position(token: Token): Position.T = position(advance(token).offset)

    override def toString: String = Position.here_undelimited(position())
  }

  abstract class Reader extends scala.util.parsing.input.Reader[Token]

  private class Token_Reader(tokens: List[Token], val pos: Pos) extends Reader
  {
    def first = tokens.head
    def rest = new Token_Reader(tokens.tail, pos.advance(first))
    def atEnd = tokens.isEmpty
  }

  def reader(tokens: List[Token], start: Token.Pos): Reader =
    new Token_Reader(tokens, start)
}


sealed case class Token(kind: Token.Kind.Value, source: String)
{
  def is_command: Boolean = kind == Token.Kind.COMMAND
  def is_command_kind(keywords: Keyword.Keywords, pred: String => Boolean): Boolean =
    is_command && keywords.is_command_kind(source, pred)
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
  def is_end: Boolean = is_command && source == "end"

  // FIXME avoid hard-wired stuff
  def is_command_modifier: Boolean =
    is_keyword && (source == "public" || source == "private" || source == "qualified")

  def is_begin_block: Boolean = is_command && source == "{"
  def is_end_block: Boolean = is_command && source == "}"

  def content: String =
    if (kind == Token.Kind.STRING) Scan.Parsers.quoted_content("\"", source)
    else if (kind == Token.Kind.ALT_STRING) Scan.Parsers.quoted_content("`", source)
    else if (kind == Token.Kind.VERBATIM) Scan.Parsers.verbatim_content(source)
    else if (kind == Token.Kind.CARTOUCHE) Scan.Parsers.cartouche_content(source)
    else if (kind == Token.Kind.COMMENT) Scan.Parsers.comment_content(source)
    else source
}
