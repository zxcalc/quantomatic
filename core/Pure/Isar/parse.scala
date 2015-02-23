/*  Title:      Pure/Isar/parse.scala
    Author:     Makarius

Generic parsers for Isabelle/Isar outer syntax.
*/

package isabelle


import scala.util.parsing.combinator.Parsers
import scala.annotation.tailrec


object Parse
{
  /* parsing tokens */

  trait Parser extends Parsers
  {
    type Elem = Token

    def filter_proper: Boolean = true

    @tailrec private def proper(in: Input): Input =
      if (!filter_proper || in.atEnd || in.first.is_proper) in
      else proper(in.rest)

    def token(s: String, pred: Elem => Boolean): Parser[(Elem, Token.Pos)] =
      new Parser[(Elem, Token.Pos)] {
        def apply(raw_input: Input) =
        {
          val in = proper(raw_input)
          if (in.atEnd) Failure(s + " expected,\nbut end-of-input was found", in)
          else {
            val pos =
              in.pos match {
                case pos: Token.Pos => pos
                case _ => Token.Pos.none
              }
            val token = in.first
            if (pred(token)) Success((token, pos), proper(in.rest))
            else
              token.text match {
                case (txt, "") =>
                  Failure(s + " expected,\nbut " + txt + " was found", in)
                case (txt1, txt2) =>
                  Failure(s + " expected,\nbut " + txt1 + " was found:\n" + txt2, in)
              }
          }
        }
      }

    def atom(s: String, pred: Elem => Boolean): Parser[String] =
      token(s, pred) ^^ { case (tok, _) => tok.content }

    def command(name: String): Parser[Position.T] =
      token("command " + quote(name), tok => tok.is_command && tok.source == name) ^^
        { case (_, pos) => pos.position }

    def keyword(name: String): Parser[String] =
      atom("keyword " + quote(name), tok => tok.is_keyword && tok.source == name)

    def string: Parser[String] = atom("string", _.is_string)
    def nat: Parser[Int] = atom("natural number", _.is_nat) ^^ (s => Integer.parseInt(s))
    def name: Parser[String] = atom("name declaration", _.is_name)
    def xname: Parser[String] = atom("name reference", _.is_xname)
    def text: Parser[String] = atom("text", _.is_text)
    def ML_source: Parser[String] = atom("ML source", _.is_text)
    def document_source: Parser[String] = atom("document source", _.is_text)
    def path: Parser[String] =
      atom("file name/path specification", tok => tok.is_name && Path.is_wellformed(tok.content))
    def theory_name: Parser[String] =
      atom("theory name", tok => tok.is_name && Path.is_wellformed(tok.content))
    def theory_xname: Parser[String] =
      atom("theory name reference", tok => tok.is_xname && Path.is_wellformed(tok.content))

    private def tag_name: Parser[String] =
      atom("tag name", tok =>
          tok.kind == Token.Kind.IDENT ||
          tok.kind == Token.Kind.STRING)

    def tags: Parser[List[String]] = rep(keyword("%") ~> tag_name)


    /* wrappers */

    def parse[T](p: Parser[T], in: Token.Reader): ParseResult[T] = p(in)

    def parse_all[T](p: Parser[T], in: Token.Reader): ParseResult[T] =
    {
      val result = parse(p, in)
      val rest = proper(result.next)
      if (result.successful && !rest.atEnd) Error("bad input", rest)
      else result
    }
  }
}

