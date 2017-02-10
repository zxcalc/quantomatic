/*  Title:      Pure/Isar/outer_syntax.scala
    Author:     Makarius

Isabelle/Isar outer syntax.
*/

package isabelle


import scala.collection.mutable


object Outer_Syntax
{
  /* syntax */

  val empty: Outer_Syntax = new Outer_Syntax()

  def init(): Outer_Syntax = new Outer_Syntax(completion = Completion.init())


  /* string literals */

  def quote_string(str: String): String =
  {
    val result = new StringBuilder(str.length + 10)
    result += '"'
    for (s <- Symbol.iterator(str)) {
      if (s.length == 1) {
        val c = s(0)
        if (c < 32 && c != YXML.X && c != YXML.Y || c == '\\' || c == '"') {
          result += '\\'
          if (c < 10) result += '0'
          if (c < 100) result += '0'
          result ++= c.asInstanceOf[Int].toString
        }
        else result += c
      }
      else result ++= s
    }
    result += '"'
    result.toString
  }
}

final class Outer_Syntax private(
  val keywords: Keyword.Keywords = Keyword.Keywords.empty,
  val completion: Completion = Completion.empty,
  val rev_abbrevs: Thy_Header.Abbrevs = Nil,
  val language_context: Completion.Language_Context = Completion.Language_Context.outer,
  val has_tokens: Boolean = true)
{
  /** syntax content **/

  override def toString: String = keywords.toString


  /* keywords */

  def + (name: String, kind: String = "", tags: List[String] = Nil): Outer_Syntax =
  {
    val keywords1 = keywords + (name, kind, tags)
    val completion1 =
      completion.add_keyword(name).
        add_abbrevs(
          (if (Keyword.theory_block.contains(kind)) List((name, name + "\nbegin\n\u0007\nend"))
           else Nil) :::
          (if (Completion.Word_Parsers.is_word(name)) List((name, name)) else Nil))
    new Outer_Syntax(keywords1, completion1, rev_abbrevs, language_context, true)
  }

  def add_keywords(keywords: Thy_Header.Keywords): Outer_Syntax =
    (this /: keywords) {
      case (syntax, (name, ((kind, tags), _))) =>
        syntax + (Symbol.decode(name), kind, tags) + (Symbol.encode(name), kind, tags)
    }


  /* abbrevs */

  def abbrevs: Thy_Header.Abbrevs = rev_abbrevs.reverse

  def add_abbrevs(new_abbrevs: Thy_Header.Abbrevs): Outer_Syntax =
    if (new_abbrevs.isEmpty) this
    else {
      val completion1 =
        completion.add_abbrevs(
          (for ((a, b) <- new_abbrevs) yield {
            val a1 = Symbol.decode(a)
            val a2 = Symbol.encode(a)
            val b1 = Symbol.decode(b)
            List((a1, b1), (a2, b1))
          }).flatten)
      val rev_abbrevs1 = Library.distinct(new_abbrevs) reverse_::: rev_abbrevs
      new Outer_Syntax(keywords, completion1, rev_abbrevs1, language_context, has_tokens)
    }


  /* merge */

  def ++ (other: Outer_Syntax): Outer_Syntax =
    if (this eq other) this
    else {
      val keywords1 = keywords ++ other.keywords
      val completion1 = completion ++ other.completion
      val rev_abbrevs1 = Library.merge(rev_abbrevs, other.rev_abbrevs)
      if ((keywords eq keywords1) && (completion eq completion1)) this
      else new Outer_Syntax(keywords1, completion1, rev_abbrevs1, language_context, has_tokens)
    }


  /* load commands */

  def load_command(name: String): Option[List[String]] = keywords.load_commands.get(name)
  def load_commands_in(text: String): Boolean = keywords.load_commands_in(text)


  /* language context */

  def set_language_context(context: Completion.Language_Context): Outer_Syntax =
    new Outer_Syntax(keywords, completion, rev_abbrevs, context, has_tokens)

  def no_tokens: Outer_Syntax =
  {
    require(keywords.is_empty)
    new Outer_Syntax(
      completion = completion,
      rev_abbrevs = rev_abbrevs,
      language_context = language_context,
      has_tokens = false)
  }



  /** parsing **/

  /* command spans */

  def parse_spans(toks: List[Token]): List[Command_Span.Span] =
  {
    val result = new mutable.ListBuffer[Command_Span.Span]
    val content = new mutable.ListBuffer[Token]
    val improper = new mutable.ListBuffer[Token]

    def ship(span: List[Token])
    {
      val kind =
        if (span.forall(_.is_improper)) Command_Span.Ignored_Span
        else if (span.exists(_.is_error)) Command_Span.Malformed_Span
        else
          span.find(_.is_command) match {
            case None => Command_Span.Malformed_Span
            case Some(cmd) =>
              val name = cmd.source
              val offset =
                (0 /: span.takeWhile(_ != cmd)) {
                  case (i, tok) => i + Symbol.iterator(tok.source).length }
              val end_offset = offset + Symbol.iterator(name).length
              val pos = Position.Range(Text.Range(offset, end_offset) + 1)
              Command_Span.Command_Span(name, pos)
          }
      result += Command_Span.Span(kind, span)
    }

    def flush()
    {
      if (content.nonEmpty) { ship(content.toList); content.clear }
      if (improper.nonEmpty) { ship(improper.toList); improper.clear }
    }

    for (tok <- toks) {
      if (tok.is_improper) improper += tok
      else if (keywords.is_before_command(tok) ||
        tok.is_command &&
          (!content.exists(keywords.is_before_command(_)) || content.exists(_.is_command)))
      { flush(); content += tok }
      else { content ++= improper; improper.clear; content += tok }
    }
    flush()

    result.toList
  }

  def parse_spans(input: CharSequence): List[Command_Span.Span] =
    parse_spans(Token.explode(keywords, input))
}
