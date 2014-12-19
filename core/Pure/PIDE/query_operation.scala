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
}

class Query_Operation[Editor_Context](
  editor: Editor[Editor_Context],
  editor_context: Editor_Context,
  operation_name: String,
  consume_status: Query_Operation.Status.Value => Unit,
  consume_output: (Document.Snapshot, Command.Results, XML.Body) => Unit)
{
  private val instance = Document_ID.make().toString


  /* implicit state -- owned by GUI thread */

  @volatile private var current_location: Option[Command] = None
  @volatile private var current_query: List[String] = Nil
  @volatile private var current_update_pending = false
  @volatile private var current_output: List[XML.Tree] = Nil
  @volatile private var current_status = Query_Operation.Status.FINISHED
  @volatile private var current_exec_id = Document_ID.none

  private def reset_state()
  {
    current_location = None
    current_query = Nil
    current_update_pending = false
    current_output = Nil
    current_status = Query_Operation.Status.FINISHED
    current_exec_id = Document_ID.none
  }

  private def remove_overlay()
  {
    current_location match {
      case None =>
      case Some(command) =>
        editor.remove_overlay(command, operation_name, instance :: current_query)
        editor.flush()
    }
  }


  /* content update */

  private def content_update()
  {
    GUI_Thread.require {}


    /* snapshot */

    val (snapshot, command_results, removed) =
      current_location match {
        case Some(cmd) =>
          val snapshot = editor.node_snapshot(cmd.node_name)
          val command_results = snapshot.state.command_results(snapshot.version, cmd)
          val removed = !snapshot.version.nodes(cmd.node_name).commands.contains(cmd)
          (snapshot, command_results, removed)
        case None =>
          (Document.Snapshot.init, Command.Results.empty, true)
      }

    val results =
      (for {
        (_, elem @ XML.Elem(Markup(Markup.RESULT, props), _)) <- command_results.iterator
        if props.contains((Markup.INSTANCE, instance))
      } yield elem).toList


    /* resolve sendback: static command id */

    def resolve_sendback(body: XML.Body): XML.Body =
    {
      current_location match {
        case None => body
        case Some(command) =>
          def resolve(body: XML.Body): XML.Body =
            body map {
              case XML.Wrapped_Elem(m, b1, b2) => XML.Wrapped_Elem(m, resolve(b1), resolve(b2))
              case XML.Elem(Markup(Markup.SENDBACK, props), b) =>
                val props1 =
                  props.map({
                    case (Markup.ID, Properties.Value.Long(id)) if id == current_exec_id =>
                      (Markup.ID, Properties.Value.Long(command.id))
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

    if (new_status == Query_Operation.Status.RUNNING)
      results.collectFirst(
      {
        case XML.Elem(Markup(_, Position.Id(id)), List(elem: XML.Elem))
        if elem.name == Markup.RUNNING => id
      }).foreach(id => current_exec_id = id)


    /* state update */

    if (current_output != new_output || current_status != new_status) {
      if (snapshot.is_outdated)
        current_update_pending = true
      else {
        current_update_pending = false
        if (current_output != new_output && !removed) {
          current_output = new_output
          consume_output(snapshot, command_results, new_output)
        }
        if (current_status != new_status) {
          current_status = new_status
          consume_status(new_status)
          if (new_status == Query_Operation.Status.FINISHED)
            remove_overlay()
        }
      }
    }
  }


  /* query operations */

  def cancel_query(): Unit =
    GUI_Thread.require { editor.session.cancel_exec(current_exec_id) }

  def apply_query(query: List[String])
  {
    GUI_Thread.require {}

    editor.current_node_snapshot(editor_context) match {
      case Some(snapshot) =>
        remove_overlay()
        reset_state()
        consume_output(Document.Snapshot.init, Command.Results.empty, Nil)
        if (!snapshot.is_outdated) {
          editor.current_command(editor_context, snapshot) match {
            case Some(command) =>
              current_location = Some(command)
              current_query = query
              current_status = Query_Operation.Status.WAITING
              editor.insert_overlay(command, operation_name, instance :: query)
            case None =>
          }
        }
        consume_status(current_status)
        editor.flush()
      case None =>
    }
  }

  def locate_query()
  {
    GUI_Thread.require {}

    for {
      command <- current_location
      snapshot = editor.node_snapshot(command.node_name)
      link <- editor.hyperlink_command(snapshot, command)
    } link.follow(editor_context)
  }


  /* main */

  private val main =
    Session.Consumer[Session.Commands_Changed](getClass.getName) {
      case changed =>
        current_location match {
          case Some(command)
          if current_update_pending ||
            (current_status != Query_Operation.Status.FINISHED &&
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
    reset_state()
    consume_output(Document.Snapshot.init, Command.Results.empty, Nil)
    consume_status(current_status)
  }
}
