/*  Title:      Pure/PIDE/command.scala
    Author:     Fabian Immler, TU Munich
    Author:     Makarius

Prover commands with accumulated results from execution.
*/

package isabelle


import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.util.parsing.input.CharSequenceReader


object Command
{
  type Edit = (Option[Command], Option[Command])

  type Blob = Exn.Result[(Document.Node.Name, Option[(SHA1.Digest, Symbol.Text_Chunk)])]
  type Blobs_Info = (List[Blob], Int)
  val no_blobs: Blobs_Info = (Nil, -1)


  /** accumulated results from prover **/

  /* results */

  object Results
  {
    type Entry = (Long, XML.Tree)
    val empty = new Results(SortedMap.empty)
    def make(args: TraversableOnce[Results.Entry]): Results = (empty /: args)(_ + _)
    def merge(args: TraversableOnce[Results]): Results = (empty /: args)(_ ++ _)
  }

  final class Results private(private val rep: SortedMap[Long, XML.Tree])
  {
    def defined(serial: Long): Boolean = rep.isDefinedAt(serial)
    def get(serial: Long): Option[XML.Tree] = rep.get(serial)
    def iterator: Iterator[Results.Entry] = rep.iterator

    def + (entry: Results.Entry): Results =
      if (defined(entry._1)) this
      else new Results(rep + entry)

    def ++ (other: Results): Results =
      if (this eq other) this
      else if (rep.isEmpty) other
      else (this /: other.iterator)(_ + _)

    override def hashCode: Int = rep.hashCode
    override def equals(that: Any): Boolean =
      that match {
        case other: Results => rep == other.rep
        case _ => false
      }
    override def toString: String = iterator.mkString("Results(", ", ", ")")
  }


  /* markup */

  object Markup_Index
  {
    val markup: Markup_Index = Markup_Index(false, Symbol.Text_Chunk.Default)
  }

  sealed case class Markup_Index(status: Boolean, chunk_name: Symbol.Text_Chunk.Name)

  object Markups
  {
    val empty: Markups = new Markups(Map.empty)

    def init(markup: Markup_Tree): Markups =
      new Markups(Map(Markup_Index.markup -> markup))
  }

  final class Markups private(private val rep: Map[Markup_Index, Markup_Tree])
  {
    def is_empty: Boolean = rep.isEmpty

    def apply(index: Markup_Index): Markup_Tree =
      rep.getOrElse(index, Markup_Tree.empty)

    def add(index: Markup_Index, markup: Text.Markup): Markups =
      new Markups(rep + (index -> (this(index) + markup)))

    def redirection_iterator: Iterator[Document_ID.Generic] =
      for (Markup_Index(_, Symbol.Text_Chunk.Id(id)) <- rep.keysIterator)
        yield id

    def redirect(other_id: Document_ID.Generic): Markups =
    {
      val rep1 =
        (for {
          (Markup_Index(status, Symbol.Text_Chunk.Id(id)), markup) <- rep.iterator
          if other_id == id
        } yield (Markup_Index(status, Symbol.Text_Chunk.Default), markup)).toMap
      if (rep1.isEmpty) Markups.empty else new Markups(rep1)
    }

    override def hashCode: Int = rep.hashCode
    override def equals(that: Any): Boolean =
      that match {
        case other: Markups => rep == other.rep
        case _ => false
      }
    override def toString: String = rep.iterator.mkString("Markups(", ", ", ")")
  }


  /* state */

  object State
  {
    def merge_results(states: List[State]): Command.Results =
      Results.merge(states.map(_.results))

    def merge_markup(states: List[State], index: Markup_Index,
        range: Text.Range, elements: Markup.Elements): Markup_Tree =
      Markup_Tree.merge(states.map(_.markup(index)), range, elements)
  }

  sealed case class State(
    command: Command,
    status: List[Markup] = Nil,
    results: Results = Results.empty,
    markups: Markups = Markups.empty)
  {
    lazy val protocol_status: Protocol.Status =
    {
      val warnings =
        if (results.iterator.exists(p => Protocol.is_warning(p._2) || Protocol.is_legacy(p._2)))
          List(Markup(Markup.WARNING, Nil))
        else Nil
      val errors =
        if (results.iterator.exists(p => Protocol.is_error(p._2)))
          List(Markup(Markup.ERROR, Nil))
        else Nil
      Protocol.Status.make((warnings ::: errors ::: status).iterator)
    }

    def markup(index: Markup_Index): Markup_Tree = markups(index)

    def redirect(other_command: Command): Option[State] =
    {
      val markups1 = markups.redirect(other_command.id)
      if (markups1.is_empty) None
      else Some(new State(other_command, Nil, Results.empty, markups1))
    }

    def eq_content(other: State): Boolean =
      command.source == other.command.source &&
      status == other.status &&
      results == other.results &&
      markups == other.markups

    private def add_status(st: Markup): State =
      copy(status = st :: status)

    private def add_markup(
      status: Boolean, chunk_name: Symbol.Text_Chunk.Name, m: Text.Markup): State =
    {
      val markups1 =
        if (status || Protocol.liberal_status_elements(m.info.name))
          markups.add(Markup_Index(true, chunk_name), m)
        else markups
      copy(markups = markups1.add(Markup_Index(false, chunk_name), m))
    }

    def accumulate(
        self_id: Document_ID.Generic => Boolean,
        other_id: Document_ID.Generic => Option[(Symbol.Text_Chunk.Id, Symbol.Text_Chunk)],
        message: XML.Elem): State =
      message match {
        case XML.Elem(Markup(Markup.STATUS, _), msgs) =>
          (this /: msgs)((state, msg) =>
            msg match {
              case elem @ XML.Elem(markup, Nil) =>
                state.
                  add_status(markup).
                  add_markup(true, Symbol.Text_Chunk.Default, Text.Info(command.proper_range, elem))
              case _ =>
                Output.warning("Ignored status message: " + msg)
                state
            })

        case XML.Elem(Markup(Markup.REPORT, _), msgs) =>
          (this /: msgs)((state, msg) =>
            {
              def bad(): Unit = Output.warning("Ignored report message: " + msg)

              msg match {
                case XML.Elem(Markup(name, atts @ Position.Identified(id, chunk_name)), args) =>

                  val target =
                    if (self_id(id) && command.chunks.isDefinedAt(chunk_name))
                      Some((chunk_name, command.chunks(chunk_name)))
                    else if (chunk_name == Symbol.Text_Chunk.Default) other_id(id)
                    else None

                  (target, atts) match {
                    case (Some((target_name, target_chunk)), Position.Range(symbol_range)) =>
                      target_chunk.incorporate(symbol_range) match {
                        case Some(range) =>
                          val props = Position.purge(atts)
                          val info = Text.Info(range, XML.Elem(Markup(name, props), args))
                          state.add_markup(false, target_name, info)
                        case None => bad(); state
                      }
                    case _ =>
                      // silently ignore excessive reports
                      state
                  }

                case XML.Elem(Markup(name, atts), args)
                if !atts.exists({ case (a, _) => Markup.POSITION_PROPERTIES(a) }) =>
                  val range = command.proper_range
                  val props = Position.purge(atts)
                  val info: Text.Markup = Text.Info(range, XML.Elem(Markup(name, props), args))
                  state.add_markup(false, Symbol.Text_Chunk.Default, info)

                case _ => bad(); state
              }
            })
        case XML.Elem(Markup(name, props), body) =>
          props match {
            case Markup.Serial(i) =>
              val message1 = XML.Elem(Markup(Markup.message(name), props), body)
              val message2 = XML.Elem(Markup(name, props), body)

              var st = copy(results = results + (i -> message1))
              if (Protocol.is_inlined(message)) {
                for {
                  (chunk_name, chunk) <- command.chunks.iterator
                  range <- Protocol_Message.positions(
                    self_id, command.span.position, chunk_name, chunk, message)
                } st = st.add_markup(false, chunk_name, Text.Info(range, message2))
              }
              st

            case _ =>
              Output.warning("Ignored message without serial number: " + message)
              this
          }
    }
  }



  /** static content **/

  /* make commands */

  def apply(
    id: Document_ID.Command,
    node_name: Document.Node.Name,
    blobs_info: Blobs_Info,
    span: Command_Span.Span): Command =
  {
    val (source, span1) = span.compact_source
    new Command(id, node_name, blobs_info, span1, source, Results.empty, Markup_Tree.empty)
  }

  val empty: Command =
    Command(Document_ID.none, Document.Node.Name.empty, no_blobs, Command_Span.empty)

  def unparsed(
    id: Document_ID.Command,
    source: String,
    results: Results,
    markup: Markup_Tree): Command =
  {
    val (source1, span1) = Command_Span.unparsed(source).compact_source
    new Command(id, Document.Node.Name.empty, no_blobs, span1, source1, results, markup)
  }

  def unparsed(source: String): Command =
    unparsed(Document_ID.none, source, Results.empty, Markup_Tree.empty)

  def rich_text(id: Document_ID.Command, results: Results, body: XML.Body): Command =
  {
    val text = XML.content(body)
    val markup = Markup_Tree.from_XML(body)
    unparsed(id, text, results, markup)
  }


  /* perspective */

  object Perspective
  {
    val empty: Perspective = Perspective(Nil)
  }

  sealed case class Perspective(commands: List[Command])  // visible commands in canonical order
  {
    def is_empty: Boolean = commands.isEmpty

    def same(that: Perspective): Boolean =
    {
      val cmds1 = this.commands
      val cmds2 = that.commands
      require(!cmds1.exists(_.is_undefined))
      require(!cmds2.exists(_.is_undefined))
      cmds1.length == cmds2.length &&
        (cmds1.iterator zip cmds2.iterator).forall({ case (c1, c2) => c1.id == c2.id })
    }
  }


  /* blobs: inlined errors and auxiliary files */

  private def clean_tokens(tokens: List[Token]): List[(Token, Int)] =
  {
    def clean(toks: List[(Token, Int)]): List[(Token, Int)] =
      toks match {
        case (t1, i1) :: (t2, i2) :: rest =>
          if (t1.is_keyword && (t1.source == "%" || t1.source == "--")) clean(rest)
          else (t1, i1) :: clean((t2, i2) :: rest)
        case _ => toks
      }
    clean(tokens.zipWithIndex.filter({ case (t, _) => t.is_proper }))
  }

  private def find_file(tokens: List[(Token, Int)]): Option[(String, Int)] =
    if (tokens.exists({ case (t, _) => t.is_command })) {
      tokens.dropWhile({ case (t, _) => !t.is_command }).
        collectFirst({ case (t, i) if t.is_name => (t.content, i) })
    }
    else None

  def span_files(syntax: Prover.Syntax, span: Command_Span.Span): (List[String], Int) =
    syntax.load_command(span.name) match {
      case Some(exts) =>
        find_file(clean_tokens(span.content)) match {
          case Some((file, i)) =>
            if (exts.isEmpty) (List(file), i)
            else (exts.map(ext => file + "." + ext), i)
          case None => (Nil, -1)
        }
      case None => (Nil, -1)
    }

  def blobs_info(
    resources: Resources,
    syntax: Prover.Syntax,
    get_blob: Document.Node.Name => Option[Document.Blob],
    can_import: Document.Node.Name => Boolean,
    node_name: Document.Node.Name,
    span: Command_Span.Span): Blobs_Info =
  {
    span.name match {
      // inlined errors
      case Thy_Header.THEORY =>
        val header =
          resources.check_thy_reader("", node_name,
            new CharSequenceReader(Token.implode(span.content)), Token.Pos.command)
        val errors =
          for ((imp, pos) <- header.imports if !can_import(imp)) yield {
            val msg =
              "Bad theory import " +
                Markup.Path(imp.node).markup(quote(imp.toString)) + Position.here(pos)
            Exn.Exn(ERROR(msg)): Command.Blob
          }
        (errors, -1)

      // auxiliary files
      case _ =>
        val (files, index) = span_files(syntax, span)
        val blobs =
          files.map(file =>
            (Exn.capture {
              val name =
                Document.Node.Name(resources.append(node_name.master_dir, Path.explode(file)))
              val blob = get_blob(name).map(blob => ((blob.bytes.sha1_digest, blob.chunk)))
              (name, blob)
            }).user_error)
        (blobs, index)
    }
  }
}


final class Command private(
    val id: Document_ID.Command,
    val node_name: Document.Node.Name,
    val blobs_info: Command.Blobs_Info,
    val span: Command_Span.Span,
    val source: String,
    val init_results: Command.Results,
    val init_markup: Markup_Tree)
{
  override def toString: String = id + "/" + span.kind.toString


  /* classification */

  def is_proper: Boolean = span.kind.isInstanceOf[Command_Span.Command_Span]
  def is_ignored: Boolean = span.kind == Command_Span.Ignored_Span

  def is_undefined: Boolean = id == Document_ID.none
  val is_unparsed: Boolean = span.content.exists(_.is_unparsed)
  val is_unfinished: Boolean = span.content.exists(_.is_unfinished)


  /* blobs */

  def blobs: List[Command.Blob] = blobs_info._1
  def blobs_index: Int = blobs_info._2

  def blobs_names: List[Document.Node.Name] =
    for (Exn.Res((name, _)) <- blobs) yield name

  def blobs_defined: List[(Document.Node.Name, SHA1.Digest)] =
    for (Exn.Res((name, Some((digest, _)))) <- blobs) yield (name, digest)

  def blobs_changed(doc_blobs: Document.Blobs): Boolean =
    blobs.exists({ case Exn.Res((name, _)) => doc_blobs.changed(name) case _ => false })


  /* source chunks */

  val chunk: Symbol.Text_Chunk = Symbol.Text_Chunk(source)

  val chunks: Map[Symbol.Text_Chunk.Name, Symbol.Text_Chunk] =
    ((Symbol.Text_Chunk.Default -> chunk) ::
      (for (Exn.Res((name, Some((_, file)))) <- blobs)
        yield Symbol.Text_Chunk.File(name.node) -> file)).toMap

  def length: Int = source.length
  def range: Text.Range = chunk.range

  val proper_range: Text.Range =
    Text.Range(0,
      (length /: span.content.reverse.iterator.takeWhile(_.is_improper))(_ - _.source.length))

  def source(range: Text.Range): String = source.substring(range.start, range.stop)


  /* accumulated results */

  val init_state: Command.State =
    Command.State(this, results = init_results, markups = Command.Markups.init(init_markup))

  val empty_state: Command.State = Command.State(this)
}
