/*  Title:      Pure/PIDE/protocol.scala
    Author:     Makarius

Protocol message formats for interactive proof documents.
*/

package isabelle


object Protocol
{
  /* document editing */

  object Assign_Update
  {
    def unapply(text: String): Option[(Document_ID.Version, Document.Assign_Update)] =
      try {
        import XML.Decode._
        val body = YXML.parse_body(text)
        Some(pair(long, list(pair(long, list(long))))(body))
      }
      catch {
        case ERROR(_) => None
        case _: XML.Error => None
      }
  }

  object Removed
  {
    def unapply(text: String): Option[List[Document_ID.Version]] =
      try {
        import XML.Decode._
        Some(list(long)(YXML.parse_body(text)))
      }
      catch {
        case ERROR(_) => None
        case _: XML.Error => None
      }
  }


  /* command status */

  object Status
  {
    def make(markup_iterator: Iterator[Markup]): Status =
    {
      var touched = false
      var accepted = false
      var warned = false
      var failed = false
      var forks = 0
      var runs = 0
      for (markup <- markup_iterator) {
        markup.name match {
          case Markup.ACCEPTED => accepted = true
          case Markup.FORKED => touched = true; forks += 1
          case Markup.JOINED => forks -= 1
          case Markup.RUNNING => touched = true; runs += 1
          case Markup.FINISHED => runs -= 1
          case Markup.WARNING => warned = true
          case Markup.FAILED | Markup.ERROR => failed = true
          case _ =>
        }
      }
      Status(touched, accepted, warned, failed, forks, runs)
    }

    val empty = make(Iterator.empty)

    def merge(status_iterator: Iterator[Status]): Status =
      if (status_iterator.hasNext) {
        val status0 = status_iterator.next
        (status0 /: status_iterator)(_ + _)
      }
      else empty
  }

  sealed case class Status(
    private val touched: Boolean,
    private val accepted: Boolean,
    private val warned: Boolean,
    private val failed: Boolean,
    forks: Int,
    runs: Int)
  {
    def + (that: Status): Status =
      Status(
        touched || that.touched,
        accepted || that.accepted,
        warned || that.warned,
        failed || that.failed,
        forks + that.forks,
        runs + that.runs)

    def is_unprocessed: Boolean = accepted && !failed && (!touched || (forks != 0 && runs == 0))
    def is_running: Boolean = runs != 0
    def is_warned: Boolean = warned
    def is_failed: Boolean = failed
    def is_finished: Boolean = !failed && touched && forks == 0 && runs == 0
  }

  val proper_status_elements =
    Markup.Elements(Markup.ACCEPTED, Markup.FORKED, Markup.JOINED, Markup.RUNNING,
      Markup.FINISHED, Markup.FAILED)

  val liberal_status_elements =
    proper_status_elements + Markup.WARNING + Markup.ERROR


  /* command timing */

  object Command_Timing
  {
    def unapply(props: Properties.T): Option[(Document_ID.Generic, isabelle.Timing)] =
      props match {
        case (Markup.FUNCTION, Markup.COMMAND_TIMING) :: args =>
          (args, args) match {
            case (Position.Id(id), Markup.Timing_Properties(timing)) => Some((id, timing))
            case _ => None
          }
        case _ => None
      }
  }


  /* node status */

  sealed case class Node_Status(
    unprocessed: Int, running: Int, warned: Int, failed: Int, finished: Int)
  {
    def total: Int = unprocessed + running + warned + failed + finished
  }

  def node_status(
    state: Document.State, version: Document.Version, node: Document.Node): Node_Status =
  {
    var unprocessed = 0
    var running = 0
    var warned = 0
    var failed = 0
    var finished = 0
    for (command <- node.commands.iterator) {
      val states = state.command_states(version, command)
      val status = Status.merge(states.iterator.map(_.protocol_status))

      if (status.is_running) running += 1
      else if (status.is_failed) failed += 1
      else if (status.is_warned) warned += 1
      else if (status.is_finished) finished += 1
      else unprocessed += 1
    }
    Node_Status(unprocessed, running, warned, failed, finished)
  }


  /* node timing */

  sealed case class Node_Timing(total: Double, commands: Map[Command, Double])

  val empty_node_timing = Node_Timing(0.0, Map.empty)

  def node_timing(
    state: Document.State,
    version: Document.Version,
    node: Document.Node,
    threshold: Double): Node_Timing =
  {
    var total = 0.0
    var commands = Map.empty[Command, Double]
    for {
      command <- node.commands.iterator
      st <- state.command_states(version, command)
    } {
      val command_timing =
        (0.0 /: st.status)({
          case (timing, Markup.Timing(t)) => timing + t.elapsed.seconds
          case (timing, _) => timing
        })
      total += command_timing
      if (command_timing >= threshold) commands += (command -> command_timing)
    }
    Node_Timing(total, commands)
  }


  /* result messages */

  private val clean_elements =
    Markup.Elements(Markup.REPORT, Markup.NO_REPORT)

  def clean_message(body: XML.Body): XML.Body =
    body filter {
      case XML.Wrapped_Elem(Markup(name, _), _, _) => !clean_elements(name)
      case XML.Elem(Markup(name, _), _) => !clean_elements(name)
      case _ => true
    } map {
      case XML.Wrapped_Elem(markup, body, ts) => XML.Wrapped_Elem(markup, body, clean_message(ts))
      case XML.Elem(markup, ts) => XML.Elem(markup, clean_message(ts))
      case t => t
    }

  def message_reports(props: Properties.T, body: XML.Body): List[XML.Elem] =
    body flatMap {
      case XML.Wrapped_Elem(Markup(Markup.REPORT, ps), body, ts) =>
        List(XML.Wrapped_Elem(Markup(Markup.REPORT, props ::: ps), body, ts))
      case XML.Elem(Markup(Markup.REPORT, ps), ts) =>
        List(XML.Elem(Markup(Markup.REPORT, props ::: ps), ts))
      case XML.Wrapped_Elem(_, _, ts) => message_reports(props, ts)
      case XML.Elem(_, ts) => message_reports(props, ts)
      case XML.Text(_) => Nil
    }


  /* specific messages */

  def is_result(msg: XML.Tree): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.RESULT, _), _) => true
      case _ => false
    }

  def is_tracing(msg: XML.Tree): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.TRACING, _), _) => true
      case XML.Elem(Markup(Markup.TRACING_MESSAGE, _), _) => true
      case _ => false
    }

  def is_writeln_markup(msg: XML.Tree, name: String): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.WRITELN, _),
        List(XML.Elem(markup, _))) => markup.name == name
      case XML.Elem(Markup(Markup.WRITELN_MESSAGE, _),
        List(XML.Elem(markup, _))) => markup.name == name
      case _ => false
    }

  def is_warning_markup(msg: XML.Tree, name: String): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.WARNING, _),
        List(XML.Elem(markup, _))) => markup.name == name
      case XML.Elem(Markup(Markup.WARNING_MESSAGE, _),
        List(XML.Elem(markup, _))) => markup.name == name
      case _ => false
    }

  def is_warning(msg: XML.Tree): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.WARNING, _), _) => true
      case XML.Elem(Markup(Markup.WARNING_MESSAGE, _), _) => true
      case _ => false
    }

  def is_error(msg: XML.Tree): Boolean =
    msg match {
      case XML.Elem(Markup(Markup.ERROR, _), _) => true
      case XML.Elem(Markup(Markup.ERROR_MESSAGE, _), _) => true
      case _ => false
    }

  def is_state(msg: XML.Tree): Boolean = is_writeln_markup(msg, Markup.STATE)
  def is_information(msg: XML.Tree): Boolean = is_writeln_markup(msg, Markup.INFORMATION)
  def is_legacy(msg: XML.Tree): Boolean = is_warning_markup(msg, Markup.LEGACY)

  def is_inlined(msg: XML.Tree): Boolean =
    !(is_result(msg) || is_tracing(msg) || is_state(msg))


  /* dialogs */

  object Dialog_Args
  {
    def unapply(props: Properties.T): Option[(Document_ID.Generic, Long, String)] =
      (props, props, props) match {
        case (Position.Id(id), Markup.Serial(serial), Markup.Result(result)) =>
          Some((id, serial, result))
        case _ => None
      }
  }

  object Dialog
  {
    def unapply(tree: XML.Tree): Option[(Document_ID.Generic, Long, String)] =
      tree match {
        case XML.Elem(Markup(Markup.DIALOG, Dialog_Args(id, serial, result)), _) =>
          Some((id, serial, result))
        case _ => None
      }
  }

  object Dialog_Result
  {
    def apply(id: Document_ID.Generic, serial: Long, result: String): XML.Elem =
    {
      val props = Position.Id(id) ::: Markup.Serial(serial)
      XML.Elem(Markup(Markup.RESULT, props), List(XML.Text(result)))
    }

    def unapply(tree: XML.Tree): Option[String] =
      tree match {
        case XML.Elem(Markup(Markup.RESULT, _), List(XML.Text(result))) => Some(result)
        case _ => None
      }
  }


  /* reported positions */

  private val position_elements =
    Markup.Elements(Markup.BINDING, Markup.ENTITY, Markup.REPORT, Markup.POSITION)

  def message_positions(
    self_id: Document_ID.Generic => Boolean,
    chunk_name: Symbol.Text_Chunk.Name,
    chunk: Symbol.Text_Chunk,
    message: XML.Elem): Set[Text.Range] =
  {
    def elem_positions(props: Properties.T, set: Set[Text.Range]): Set[Text.Range] =
      props match {
        case Position.Reported(id, name, symbol_range)
        if self_id(id) && name == chunk_name =>
          chunk.incorporate(symbol_range) match {
            case Some(range) => set + range
            case _ => set
          }
        case _ => set
      }

    def positions(set: Set[Text.Range], tree: XML.Tree): Set[Text.Range] =
      tree match {
        case XML.Wrapped_Elem(Markup(name, props), _, body) =>
          body.foldLeft(if (position_elements(name)) elem_positions(props, set) else set)(positions)
        case XML.Elem(Markup(name, props), body) =>
          body.foldLeft(if (position_elements(name)) elem_positions(props, set) else set)(positions)
        case XML.Text(_) => set
      }

    val set = positions(Set.empty, message)
    if (set.isEmpty) elem_positions(message.markup.properties, set)
    else set
  }
}


trait Protocol extends Prover
{
  /* options */

  def options(opts: Options): Unit =
    protocol_command("Prover.options", YXML.string_of_body(opts.encode))


  /* interned items */

  def define_blob(digest: SHA1.Digest, bytes: Bytes): Unit =
    protocol_command_bytes("Document.define_blob", Bytes(digest.toString), bytes)

  def define_command(command: Command): Unit =
  {
    val blobs_yxml =
    { import XML.Encode._
      val encode_blob: T[Command.Blob] =
        variant(List(
          { case Exn.Res((a, b)) =>
              (Nil, pair(string, option(string))((a.node, b.map(p => p._1.toString)))) },
          { case Exn.Exn(e) => (Nil, string(Exn.message(e))) }))
      YXML.string_of_body(list(encode_blob)(command.blobs))
    }
    protocol_command("Document.define_command",
      Document_ID(command.id), encode(command.name), blobs_yxml, encode(command.source))
  }


  /* execution */

  def discontinue_execution(): Unit =
    protocol_command("Document.discontinue_execution")

  def cancel_exec(id: Document_ID.Exec): Unit =
    protocol_command("Document.cancel_exec", Document_ID(id))


  /* document versions */

  def update(old_id: Document_ID.Version, new_id: Document_ID.Version,
    edits: List[Document.Edit_Command])
  {
    val edits_yxml =
    { import XML.Encode._
      def id: T[Command] = (cmd => long(cmd.id))
      def encode_edit(name: Document.Node.Name)
          : T[Document.Node.Edit[Command.Edit, Command.Perspective]] =
        variant(List(
          { case Document.Node.Edits(a) => (Nil, list(pair(option(id), option(id)))(a)) },
          { case Document.Node.Deps(header) =>
              val master_dir = Isabelle_System.posix_path_url(name.master_dir)
              val theory = Long_Name.base_name(name.theory)
              val imports = header.imports.map(_.node)
              val keywords = header.keywords.map({ case (a, b, _) => (a, b) })
              (Nil,
                pair(Encode.string, pair(Encode.string, pair(list(Encode.string),
                  pair(list(pair(Encode.string,
                    option(pair(pair(Encode.string, list(Encode.string)), list(Encode.string))))),
                  list(Encode.string)))))(
                (master_dir, (theory, (imports, (keywords, header.errors)))))) },
          { case Document.Node.Perspective(a, b, c) =>
              (bool_atom(a) :: b.commands.map(cmd => long_atom(cmd.id)),
                list(pair(id, pair(Encode.string, list(Encode.string))))(c.dest)) }))
      def encode_edits: T[List[Document.Edit_Command]] = list((node_edit: Document.Edit_Command) =>
      {
        val (name, edit) = node_edit
        pair(string, encode_edit(name))(name.node, edit)
      })
      YXML.string_of_body(encode_edits(edits)) }
    protocol_command("Document.update", Document_ID(old_id), Document_ID(new_id), edits_yxml)
  }

  def remove_versions(versions: List[Document.Version])
  {
    val versions_yxml =
      { import XML.Encode._
        YXML.string_of_body(list(long)(versions.map(_.id))) }
    protocol_command("Document.remove_versions", versions_yxml)
  }


  /* dialog via document content */

  def dialog_result(serial: Long, result: String): Unit =
    protocol_command("Document.dialog_result", Properties.Value.Long(serial), result)


  /* use_theories */

  def use_theories(id: String, master_dir: Path, thys: List[Path]): Unit =
    protocol_command("use_theories", (id :: master_dir.implode :: thys.map(_.implode)): _*)
}
