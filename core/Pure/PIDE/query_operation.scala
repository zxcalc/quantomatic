/*  Title:      Pure/PIDE/query_operation.scala
    Author:     Makarius

One-shot query operations via asynchronous print functions and temporary
document overlays.
*/

package isabelle


object Query_Operation
{
  object Status extends Enumeration
  {
    val WAITING = Value("waiting")
    val RUNNING = Value("running")
    val FINISHED = Value("finished")
  }

  object State
  {
    val empty: State = State()

    def make(command: Command, query: List[String]): State =
      State(instance = Document_ID.make().toString,
        location = Some(command),
        query = query,
        status = Status.WAITING)
  }

  sealed case class State(
    instance: String = Document_ID.none.toString,
    location: Option[Command] = None,
    query: List[String] = Nil,
    update_pending: Boolean = false,
    output: List[XML.Tree] = Nil,
    status: Status.Value = Status.FINISHED,
    exec_id: Document_ID.Exec = Document_ID.none)
}

class Query_Operation[Editor_Context](
  editor: Editor[Editor_Context],
  editor_context: Editor_Context,
  operation_name: String,
  consume_status: Query_Operation.Status.Value => Unit,
  consume_output: (Document.Snapshot, Command.Results, XML.Body) => Unit)
{
  private val print_function = operation_name + "_query"


  /* implicit state -- owned by GUI thread */

  private val current_state = Synchronized(Query_Operation.State.empty)

  def get_location: Option[Command] = current_state.value.location

  private def remove_overlay()
  {
    val state = current_state.value
    for (command <- state.location) {
      editor.remove_overlay(command, print_function, state.instance :: state.query)
    }
  }


  /* content update */

  private def content_update()
  {
    GUI_Thread.require {}


    /* snapshot */

    val state0 = current_state.value

    val (snapshot, command_results, results, removed) =
      state0.location match {
        case Some(cmd) =>
          val snapshot = editor.node_snapshot(cmd.node_name)
          val command_results = snapshot.state.command_results(snapshot.version, cmd)
          val results =
            (for {
              (_, elem @ XML.Elem(Markup(Markup.RESULT, props), _)) <- command_results.iterator
              if props.contains((Markup.INSTANCE, state0.instance))
            } yield elem).toList
          val removed = !snapshot.version.nodes(cmd.node_name).commands.contains(cmd)
          (snapshot, command_results, results, removed)
        case None =>
          (Document.Snapshot.init, Command.Results.empty, Nil, true)
      }



    /* resolve sendback: static command id */

    def resolve_sendback(body: XML.Body): XML.Body =
    {
      state0.location match {
        case None => body
        case Some(command) =>
          def resolve(body: XML.Body): XML.Body =
            body map {
              case XML.Wrapped_Elem(m, b1, b2) => XML.Wrapped_Elem(m, resolve(b1), resolve(b2))
              case XML.Elem(Markup(Markup.SENDBACK, props), b) =>
                val props1 =
                  props.map({
                    case (Markup.ID, Value.Long(id)) if id == state0.exec_id =>
                      (Markup.ID, Value.Long(command.id))
                    case p => p
                  })
                XML.Elem(Markup(Markup.SENDBACK, props1), resolve(b))
              case XML.Elem(m, b) => XML.Elem(m, resolve(b))
              case t => t
            }
          resolve(body)
      }
    }


    /* output */

    val new_output =
      for {
        XML.Elem(_, List(XML.Elem(markup, body))) <- results
        if Markup.messages.contains(markup.name)
        body1 = resolve_sendback(body)
      } yield XML.Elem(Markup(Markup.message(markup.name), markup.properties), body1)


    /* status */

    def get_status(name: String, status: Query_Operation.Status.Value)
        : Option[Query_Operation.Status.Value] =
      results.collectFirst({ case XML.Elem(_, List(elem: XML.Elem)) if elem.name == name => status })

    val new_status =
      if (removed) Query_Operation.Status.FINISHED
      else
        get_status(Markup.FINISHED, Query_Operation.Status.FINISHED) orElse
        get_status(Markup.RUNNING, Query_Operation.Status.RUNNING) getOrElse
        Query_Operation.Status.WAITING


    /* state update */

    if (new_status == Query_Operation.Status.RUNNING)
      results.collectFirst(
      {
        case XML.Elem(Markup(_, Position.Id(id)), List(elem: XML.Elem))
        if elem.name == Markup.RUNNING => id
      }).foreach(id => current_state.change(_.copy(exec_id = id)))

    if (state0.output != new_output || state0.status != new_status) {
      if (snapshot.is_outdated)
        current_state.change(_.copy(update_pending = true))
      else {
        current_state.change(_.copy(update_pending = false))
        if (state0.output != new_output && !removed) {
          current_state.change(_.copy(output = new_output))
          consume_output(snapshot, command_results, new_output)
        }
        if (state0.status != new_status) {
          current_state.change(_.copy(status = new_status))
          consume_status(new_status)
          if (new_status == Query_Operation.Status.FINISHED)
            remove_overlay()
        }
      }
    }
  }


  /* query operations */

  def cancel_query(): Unit =
    GUI_Thread.require { editor.session.cancel_exec(current_state.value.exec_id) }

  def apply_query(query: List[String])
  {
    GUI_Thread.require {}

    editor.current_node_snapshot(editor_context) match {
      case Some(snapshot) =>
        remove_overlay()
        current_state.change(_ => Query_Operation.State.empty)
        consume_output(Document.Snapshot.init, Command.Results.empty, Nil)
        if (!snapshot.is_outdated) {
          editor.current_command(editor_context, snapshot) match {
            case Some(command) =>
              val state = Query_Operation.State.make(command, query)
              current_state.change(_ => state)
              editor.insert_overlay(command, print_function, state.instance :: query)
            case None =>
          }
        }
        consume_status(current_state.value.status)
        editor.flush()
      case None =>
    }
  }

  def locate_query()
  {
    GUI_Thread.require {}

    val state = current_state.value
    for {
      command <- state.location
      snapshot = editor.node_snapshot(command.node_name)
      link <- editor.hyperlink_command(true, snapshot, command)
    } link.follow(editor_context)
  }


  /* main */

  private val main =
    Session.Consumer[Session.Commands_Changed](getClass.getName) {
      case changed =>
        val state = current_state.value
        state.location match {
          case Some(command)
          if state.update_pending ||
            (state.status != Query_Operation.Status.FINISHED &&
              changed.commands.contains(command)) =>
            GUI_Thread.later { content_update() }
          case _ =>
        }
    }

  def activate() {
    editor.session.commands_changed += main
  }

  def deactivate() {
    editor.session.commands_changed -= main
    remove_overlay()
    current_state.change(_ => Query_Operation.State.empty)
    consume_output(Document.Snapshot.init, Command.Results.empty, Nil)
    consume_status(Query_Operation.Status.FINISHED)
  }
}
