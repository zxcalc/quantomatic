/*  Title:      Pure/PIDE/session.scala
    Author:     Makarius
    Options:    :folding=explicit:collapseFolds=1:

PIDE editor session, potentially with running prover process.
*/

package isabelle


import scala.collection.immutable.Queue


object Session
{
  /* outlets */

  object Consumer
  {
    def apply[A](name: String)(consume: A => Unit): Consumer[A] =
      new Consumer[A](name, consume)
  }
  final class Consumer[-A] private(val name: String, val consume: A => Unit)

  class Outlet[A](dispatcher: Consumer_Thread[() => Unit])
  {
    private val consumers = Synchronized(List.empty[Consumer[A]])

    def += (c: Consumer[A]) { consumers.change(Library.update(c)) }
    def -= (c: Consumer[A]) { consumers.change(Library.remove(c)) }

    def post(a: A)
    {
      for (c <- consumers.value.iterator) {
        dispatcher.send(() =>
          try { c.consume(a) }
          catch {
            case exn: Throwable =>
              Output.error_message("Consumer failed: " + quote(c.name) + "\n" + Exn.message(exn))
          })
      }
    }
  }


  /* change */

  sealed case class Change(
    previous: Document.Version,
    syntax_changed: Boolean,
    deps_changed: Boolean,
    doc_edits: List[Document.Edit_Command],
    version: Document.Version)

  case object Change_Flush


  /* events */

  //{{{
  case class Statistics(props: Properties.T)
  case class Global_Options(options: Options)
  case object Caret_Focus
  case class Raw_Edits(doc_blobs: Document.Blobs, edits: List[Document.Edit_Text])
  case class Dialog_Result(id: Document_ID.Generic, serial: Long, result: String)
  case class Commands_Changed(
    assignment: Boolean, nodes: Set[Document.Node.Name], commands: Set[Command])

  sealed abstract class Phase
  case object Inactive extends Phase
  case object Startup extends Phase  // transient
  case object Failed extends Phase
  case object Ready extends Phase
  case object Shutdown extends Phase  // transient
  //}}}


  /* syslog */

  private[Session] class Syslog(limit: Int)
  {
    private var queue = Queue.empty[XML.Elem]
    private var length = 0

    def += (msg: XML.Elem): Unit = synchronized {
      queue = queue.enqueue(msg)
      length += 1
      if (length > limit) queue = queue.dequeue._2
    }

    def content: String = synchronized {
      cat_lines(queue.iterator.map(XML.content)) +
      (if (length > limit) "\n(A total of " + length + " messages...)" else "")
    }
  }


  /* protocol handlers */

  abstract class Protocol_Handler
  {
    def start(prover: Prover): Unit = {}
    def stop(prover: Prover): Unit = {}
    val functions: Map[String, (Prover, Prover.Protocol_Output) => Boolean]
  }

  class Protocol_Handlers(
    handlers: Map[String, Session.Protocol_Handler] = Map.empty,
    functions: Map[String, Prover.Protocol_Output => Boolean] = Map.empty)
  {
    def get(name: String): Option[Protocol_Handler] = handlers.get(name)

    def add(prover: Prover, name: String): Protocol_Handlers =
    {
      val (handlers1, functions1) =
        handlers.get(name) match {
          case Some(old_handler) =>
            Output.warning("Redefining protocol handler: " + name)
            old_handler.stop(prover)
            (handlers - name, functions -- old_handler.functions.keys)
          case None => (handlers, functions)
        }

      val (handlers2, functions2) =
        try {
          val new_handler = Class.forName(name).newInstance.asInstanceOf[Protocol_Handler]
          new_handler.start(prover)

          val new_functions =
            for ((a, f) <- new_handler.functions.toList) yield
              (a, (msg: Prover.Protocol_Output) => f(prover, msg))

          val dups = for ((a, _) <- new_functions if functions1.isDefinedAt(a)) yield a
          if (!dups.isEmpty) error("Duplicate protocol functions: " + commas_quote(dups))

          (handlers1 + (name -> new_handler), functions1 ++ new_functions)
        }
        catch {
          case exn: Throwable =>
            Output.error_message(
              "Failed to initialize protocol handler: " + quote(name) + "\n" + Exn.message(exn))
            (handlers1, functions1)
        }

      new Protocol_Handlers(handlers2, functions2)
    }

    def invoke(msg: Prover.Protocol_Output): Boolean =
      msg.properties match {
        case Markup.Function(a) if functions.isDefinedAt(a) =>
          try { functions(a)(msg) }
          catch {
            case exn: Throwable =>
              Output.error_message(
                "Failed invocation of protocol function: " + quote(a) + "\n" + Exn.message(exn))
            false
          }
        case _ => false
      }

    def stop(prover: Prover): Protocol_Handlers =
    {
      for ((_, handler) <- handlers) handler.stop(prover)
      new Protocol_Handlers()
    }
  }
}


class Session(val resources: Resources)
{
  /* global flags */

  @volatile var timing: Boolean = false
  @volatile var verbose: Boolean = false


  /* tuning parameters */

  def output_delay: Time = Time.seconds(0.1)  // prover output (markup, common messages)
  def prune_delay: Time = Time.seconds(60.0)  // prune history (delete old versions)
  def prune_size: Int = 0  // size of retained history
  def syslog_limit: Int = 100
  def reparse_limit: Int = 0


  /* outlets */

  private val dispatcher =
    Consumer_Thread.fork[() => Unit]("Session.dispatcher", daemon = true) { case e => e(); true }

  val statistics = new Session.Outlet[Session.Statistics](dispatcher)
  val global_options = new Session.Outlet[Session.Global_Options](dispatcher)
  val caret_focus = new Session.Outlet[Session.Caret_Focus.type](dispatcher)
  val raw_edits = new Session.Outlet[Session.Raw_Edits](dispatcher)
  val commands_changed = new Session.Outlet[Session.Commands_Changed](dispatcher)
  val phase_changed = new Session.Outlet[Session.Phase](dispatcher)
  val syslog_messages = new Session.Outlet[Prover.Output](dispatcher)
  val raw_output_messages = new Session.Outlet[Prover.Output](dispatcher)
  val all_messages = new Session.Outlet[Prover.Message](dispatcher)  // potential bottle-neck
  val trace_events = new Session.Outlet[Simplifier_Trace.Event.type](dispatcher)



  /** main protocol manager **/

  /* internal messages */

  private case class Start(name: String, args: List[String])
  private case object Stop
  private case class Cancel_Exec(exec_id: Document_ID.Exec)
  private case class Protocol_Command(name: String, args: List[String])
  private case class Update_Options(options: Options)
  private case object Prune_History


  /* global state */

  private val syslog = new Session.Syslog(syslog_limit)
  def syslog_content(): String = syslog.content

  @volatile private var _phase: Session.Phase = Session.Inactive
  private def phase_=(new_phase: Session.Phase)
  {
    _phase = new_phase
    phase_changed.post(new_phase)
  }
  def phase = _phase
  def is_ready: Boolean = phase == Session.Ready

  private val global_state = Synchronized(Document.State.init)
  def current_state(): Document.State = global_state.value

  def recent_syntax(): Prover.Syntax =
  {
    val version = current_state().recent_finished.version.get_finished
    version.syntax getOrElse resources.base_syntax
  }


  /* protocol handlers */

  @volatile private var _protocol_handlers = new Session.Protocol_Handlers()

  def protocol_handler(name: String): Option[Session.Protocol_Handler] =
    _protocol_handlers.get(name)


  /* theory files */

  def header_edit(name: Document.Node.Name, header: Document.Node.Header): Document.Edit_Text =
  {
    val header1 =
      if (resources.loaded_theories(name.theory))
        header.error("Cannot update finished theory " + quote(name.theory))
      else header
    (name, Document.Node.Deps(header1))
  }


  /* pipelined change parsing */

  private case class Text_Edits(
    previous: Future[Document.Version],
    doc_blobs: Document.Blobs,
    text_edits: List[Document.Edit_Text],
    version_result: Promise[Document.Version])

  private val change_parser = Consumer_Thread.fork[Text_Edits]("change_parser", daemon = true)
  {
    case Text_Edits(previous, doc_blobs, text_edits, version_result) =>
      val prev = previous.get_finished
      val change =
        Timing.timeit("parse_change", timing) {
          resources.parse_change(reparse_limit, prev, doc_blobs, text_edits)
        }
      version_result.fulfill(change.version)
      manager.send(change)
      true
  }


  /* buffered changes */

  private object change_buffer
  {
    private var assignment: Boolean = false
    private var nodes: Set[Document.Node.Name] = Set.empty
    private var commands: Set[Command] = Set.empty

    def flush(): Unit = synchronized {
      if (assignment || !nodes.isEmpty || !commands.isEmpty)
        commands_changed.post(Session.Commands_Changed(assignment, nodes, commands))
      assignment = false
      nodes = Set.empty
      commands = Set.empty
    }
    private val delay_flush = Simple_Thread.delay_first(output_delay) { flush() }

    def invoke(assign: Boolean, cmds: List[Command]): Unit = synchronized {
      assignment |= assign
      for (command <- cmds) {
        nodes += command.node_name
        commands += command
      }
      delay_flush.invoke()
    }

    def shutdown()
    {
      delay_flush.revoke()
      flush()
    }
  }


  /* postponed changes */

  private object postponed_changes
  {
    private var postponed: List[Session.Change] = Nil

    def store(change: Session.Change): Unit = synchronized { postponed ::= change }

    def flush(state: Document.State): List[Session.Change] = synchronized {
      val (assigned, unassigned) = postponed.partition(change => state.is_assigned(change.previous))
      postponed = unassigned
      assigned.reverse
    }
  }


  /* prover process */

  private object prover
  {
    private val variable = Synchronized(None: Option[Prover])

    def defined: Boolean = variable.value.isDefined
    def get: Prover = variable.value.get
    def set(p: Prover) { variable.change(_ => Some(p)) }
    def reset { variable.change(_ => None) }
    def await_reset() { variable.guarded_access({ case None => Some((), None) case _ => None }) }
  }


  /* manager thread */

  private val delay_prune = Simple_Thread.delay_first(prune_delay) { manager.send(Prune_History) }

  private val manager: Consumer_Thread[Any] =
  {
    /* raw edits */

    def handle_raw_edits(doc_blobs: Document.Blobs, edits: List[Document.Edit_Text])
    //{{{
    {
      require(prover.defined)

      prover.get.discontinue_execution()

      val previous = global_state.value.history.tip.version
      val version = Future.promise[Document.Version]
      global_state.change(_.continue_history(previous, edits, version))

      raw_edits.post(Session.Raw_Edits(doc_blobs, edits))
      change_parser.send(Text_Edits(previous, doc_blobs, edits, version))
    }
    //}}}


    /* resulting changes */

    def handle_change(change: Session.Change)
    //{{{
    {
      require(prover.defined)

      def id_command(command: Command)
      {
        for {
          (name, digest) <- command.blobs_defined
          if !global_state.value.defined_blob(digest)
        } {
          change.version.nodes(name).get_blob match {
            case Some(blob) =>
              global_state.change(_.define_blob(digest))
              prover.get.define_blob(digest, blob.bytes)
            case None =>
              Output.error_message("Missing blob " + quote(name.toString))
          }
        }

        if (!global_state.value.defined_command(command.id)) {
          global_state.change(_.define_command(command))
          prover.get.define_command(command)
        }
      }
      change.doc_edits foreach {
        case (_, edit) =>
          edit foreach { case (c1, c2) => c1 foreach id_command; c2 foreach id_command }
      }

      val assignment = global_state.value.the_assignment(change.previous).check_finished
      global_state.change(_.define_version(change.version, assignment))
      prover.get.update(change.previous.id, change.version.id, change.doc_edits)
      resources.commit(change)
    }
    //}}}


    /* prover output */

    def handle_output(output: Prover.Output)
    //{{{
    {
      def bad_output()
      {
        if (verbose)
          Output.warning("Ignoring bad prover output: " + output.message.toString)
      }

      def accumulate(state_id: Document_ID.Generic, message: XML.Elem)
      {
        try {
          val st = global_state.change_result(_.accumulate(state_id, message))
          change_buffer.invoke(false, List(st.command))
        }
        catch {
          case _: Document.State.Fail => bad_output()
        }
      }

      output match {
        case msg: Prover.Protocol_Output =>
          val handled = _protocol_handlers.invoke(msg)
          if (!handled) {
            msg.properties match {
              case Markup.Protocol_Handler(name) if prover.defined =>
                _protocol_handlers = _protocol_handlers.add(prover.get, name)

              case Protocol.Command_Timing(state_id, timing) if prover.defined =>
                val message = XML.elem(Markup.STATUS, List(XML.Elem(Markup.Timing(timing), Nil)))
                accumulate(state_id, prover.get.xml_cache.elem(message))

              case Markup.Assign_Update =>
                msg.text match {
                  case Protocol.Assign_Update(id, update) =>
                    try {
                      val cmds = global_state.change_result(_.assign(id, update))
                      change_buffer.invoke(true, cmds)
                      manager.send(Session.Change_Flush)
                    }
                    catch { case _: Document.State.Fail => bad_output() }
                  case _ => bad_output()
                }
                delay_prune.invoke()

              case Markup.Removed_Versions =>
                msg.text match {
                  case Protocol.Removed(removed) =>
                    try {
                      global_state.change(_.removed_versions(removed))
                      manager.send(Session.Change_Flush)
                    }
                    catch { case _: Document.State.Fail => bad_output() }
                  case _ => bad_output()
                }

              case Markup.ML_Statistics(props) =>
                statistics.post(Session.Statistics(props))

              case Markup.Task_Statistics(props) =>
                // FIXME

              case _ => bad_output()
            }
          }
        case _ =>
          output.properties match {
            case Position.Id(state_id) =>
              accumulate(state_id, output.message)

            case _ if output.is_init =>
              phase = Session.Ready

            case Markup.Return_Code(rc) if output.is_exit =>
              if (rc == 0) phase = Session.Inactive
              else phase = Session.Failed
              prover.reset

            case _ => raw_output_messages.post(output)
          }
        }
    }
    //}}}


    /* main thread */

    Consumer_Thread.fork[Any]("Session.manager", daemon = true)
    {
      case arg: Any =>
        //{{{
        arg match {
          case output: Prover.Output =>
            if (output.is_stdout || output.is_stderr)
              raw_output_messages.post(output)
            else handle_output(output)

            if (output.is_syslog) {
              syslog += output.message
              syslog_messages.post(output)
            }

            all_messages.post(output)

          case input: Prover.Input =>
            all_messages.post(input)

          case Start(name, args) if !prover.defined =>
            if (phase == Session.Inactive || phase == Session.Failed) {
              phase = Session.Startup
              prover.set(resources.start_prover(manager.send(_), name, args))
            }

          case Stop =>
            if (prover.defined && is_ready) {
              _protocol_handlers = _protocol_handlers.stop(prover.get)
              global_state.change(_ => Document.State.init)
              phase = Session.Shutdown
              prover.get.terminate
            }

          case Prune_History =>
            if (prover.defined) {
              val old_versions = global_state.change_result(_.remove_versions(prune_size))
              if (!old_versions.isEmpty) prover.get.remove_versions(old_versions)
            }

          case Update_Options(options) =>
            if (prover.defined && is_ready) {
              prover.get.options(options)
              handle_raw_edits(Document.Blobs.empty, Nil)
            }
            global_options.post(Session.Global_Options(options))

          case Cancel_Exec(exec_id) if prover.defined =>
            prover.get.cancel_exec(exec_id)

          case Session.Raw_Edits(doc_blobs, edits) if prover.defined =>
            handle_raw_edits(doc_blobs, edits)

          case Session.Dialog_Result(id, serial, result) if prover.defined =>
            prover.get.dialog_result(serial, result)
            handle_output(new Prover.Output(Protocol.Dialog_Result(id, serial, result)))

          case Protocol_Command(name, args) if prover.defined =>
            prover.get.protocol_command(name, args:_*)

          case change: Session.Change if prover.defined =>
            val state = global_state.value
            if (!state.removing_versions && state.is_assigned(change.previous))
              handle_change(change)
            else postponed_changes.store(change)

          case Session.Change_Flush if prover.defined =>
            val state = global_state.value
            if (!state.removing_versions)
              postponed_changes.flush(state).foreach(handle_change(_))

          case bad =>
            if (verbose) Output.warning("Ignoring bad message: " + bad.toString)
        }
        true
        //}}}
    }
  }


  /* main operations */

  def snapshot(name: Document.Node.Name = Document.Node.Name.empty,
      pending_edits: List[Text.Edit] = Nil): Document.Snapshot =
    global_state.value.snapshot(name, pending_edits)

  def start(name: String, args: List[String])
  { manager.send(Start(name, args)) }

  def stop()
  {
    manager.send_wait(Stop)
    prover.await_reset()
    change_parser.shutdown()
    change_buffer.shutdown()
    delay_prune.revoke()
    manager.shutdown()
    dispatcher.shutdown()
  }

  def protocol_command(name: String, args: String*)
  { manager.send(Protocol_Command(name, args.toList)) }

  def cancel_exec(exec_id: Document_ID.Exec)
  { manager.send(Cancel_Exec(exec_id)) }

  def update(doc_blobs: Document.Blobs, edits: List[Document.Edit_Text])
  { if (!edits.isEmpty) manager.send_wait(Session.Raw_Edits(doc_blobs, edits)) }

  def update_options(options: Options)
  { manager.send_wait(Update_Options(options)) }

  def dialog_result(id: Document_ID.Generic, serial: Long, result: String)
  { manager.send(Session.Dialog_Result(id, serial, result)) }
}
