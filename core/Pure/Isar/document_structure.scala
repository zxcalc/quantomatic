/*  Title:      Pure/Isar/document_structure.scala
    Author:     Makarius

Overall document structure.
*/

package isabelle


import scala.collection.mutable
import scala.annotation.tailrec


object Document_Structure
{
  /** general structure **/

  sealed abstract class Document { def length: Int }
  case class Block(name: String, text: String, body: List[Document]) extends Document
  { val length: Int = (0 /: body)(_ + _.length) }
  case class Atom(length: Int) extends Document

  private def is_theory_command(keywords: Keyword.Keywords, name: String): Boolean =
    keywords.kinds.get(name) match {
      case Some(kind) => Keyword.theory(kind) && !Keyword.theory_end(kind)
      case None => false
    }



  /** context blocks **/

  def parse_blocks(
    syntax: Outer_Syntax,
    node_name: Document.Node.Name,
    text: CharSequence): List[Document] =
  {
    def is_plain_theory(command: Command): Boolean =
      is_theory_command(syntax.keywords, command.span.name) &&
      !command.span.is_begin && !command.span.is_end


    /* stack operations */

    def buffer(): mutable.ListBuffer[Document] = new mutable.ListBuffer[Document]

    var stack: List[(Command, mutable.ListBuffer[Document])] =
      List((Command.empty, buffer()))

    def open(command: Command) { stack = (command, buffer()) :: stack }

    def close(): Boolean =
      stack match {
        case (command, body) :: (_, body2) :: _ =>
          body2 += Block(command.span.name, command.source, body.toList)
          stack = stack.tail
          true
        case _ =>
          false
      }

    def flush() { if (is_plain_theory(stack.head._1)) close() }

    def result(): List[Document] =
    {
      while (close()) { }
      stack.head._2.toList
    }

    def add(command: Command)
    {
      if (command.span.is_begin || is_plain_theory(command)) { flush(); open(command) }
      else if (command.span.is_end) { flush(); close() }

      stack.head._2 += Atom(command.source.length)
    }


    /* result structure */

    val spans = syntax.parse_spans(text)
    spans.foreach(span => add(Command(Document_ID.none, node_name, Command.no_blobs, span)))
    result()
  }



  /** section headings **/

  trait Item
  {
    def name: String = ""
    def source: String = ""
    def heading_level: Option[Int] = None
  }

  object No_Item extends Item

  class Sections(keywords: Keyword.Keywords)
  {
    private def buffer(): mutable.ListBuffer[Document] = new mutable.ListBuffer[Document]

    private var stack: List[(Int, Item, mutable.ListBuffer[Document])] =
      List((0, No_Item, buffer()))

    @tailrec private def close(level: Int => Boolean)
    {
      stack match {
        case (lev, item, body) :: (_, _, body2) :: _ if level(lev) =>
          body2 += Block(item.name, item.source, body.toList)
          stack = stack.tail
          close(level)
        case _ =>
      }
    }

    def result(): List[Document] =
    {
      close(_ => true)
      stack.head._3.toList
    }

    def add(item: Item)
    {
      item.heading_level match {
        case Some(i) =>
          close(_ > i)
          stack = (i + 1, item, buffer()) :: stack
        case None =>
      }
      stack.head._3 += Atom(item.source.length)
    }
  }


  /* outer syntax sections */

  private class Command_Item(keywords: Keyword.Keywords, command: Command) extends Item
  {
    override def name: String = command.span.name
    override def source: String = command.source
    override def heading_level: Option[Int] =
    {
      name match {
        case Thy_Header.CHAPTER => Some(0)
        case Thy_Header.SECTION => Some(1)
        case Thy_Header.SUBSECTION => Some(2)
        case Thy_Header.SUBSUBSECTION => Some(3)
        case Thy_Header.PARAGRAPH => Some(4)
        case Thy_Header.SUBPARAGRAPH => Some(5)
        case _ if is_theory_command(keywords, name) => Some(6)
        case _ => None
      }
    }
  }

  def parse_sections(
    syntax: Outer_Syntax,
    node_name: Document.Node.Name,
    text: CharSequence): List[Document] =
  {
    val sections = new Sections(syntax.keywords)

    for { span <- syntax.parse_spans(text) } {
      sections.add(
        new Command_Item(syntax.keywords,
          Command(Document_ID.none, node_name, Command.no_blobs, span)))
    }
    sections.result()
  }


  /* ML sections */

  private class ML_Item(token: ML_Lex.Token, level: Option[Int]) extends Item
  {
    override def source: String = token.source
    override def heading_level: Option[Int] = level
  }

  def parse_ml_sections(SML: Boolean, text: CharSequence): List[Document] =
  {
    val sections = new Sections(Keyword.Keywords.empty)
    val nl = new ML_Item(ML_Lex.Token(ML_Lex.Kind.SPACE, "\n"), None)

    var context: Scan.Line_Context = Scan.Finished
    for (line <- Library.separated_chunks(_ == '\n', text)) {
      val (toks, next_context) = ML_Lex.tokenize_line(SML, line, context)
      val level =
        toks.filterNot(_.is_space) match {
          case List(tok) if tok.is_comment =>
            val s = tok.source
            if (Word.codepoint_iterator(s).exists(c =>
                  Character.isLetter(c) || Character.isDigit(c)))
            {
              if (s.startsWith("(**** ") && s.endsWith(" ****)")) Some(0)
              else if (s.startsWith("(*** ") && s.endsWith(" ***)")) Some(1)
              else if (s.startsWith("(** ") && s.endsWith(" **)")) Some(2)
              else if (s.startsWith("(* ") && s.endsWith(" *)")) Some(3)
              else None
            }
            else None
          case _ => None
        }
      if (level.isDefined && context == Scan.Finished && next_context == Scan.Finished)
        toks.foreach(tok => sections.add(new ML_Item(tok, if (tok.is_comment) level else None)))
      else
        toks.foreach(tok => sections.add(new ML_Item(tok, None)))

      sections.add(nl)
      context = next_context
    }
    sections.result()
  }
}
