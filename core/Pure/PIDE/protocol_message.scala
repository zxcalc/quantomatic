/*  Title:      Pure/PIDE/protocol_message.scala
    Author:     Makarius

Auxiliary operations on protocol messages.
*/

package isabelle


object Protocol_Message
{
  /* inlined reports */

  private val report_elements =
    Markup.Elements(Markup.REPORT, Markup.NO_REPORT)

  def clean_reports(body: XML.Body): XML.Body =
    body filter {
      case XML.Wrapped_Elem(Markup(name, _), _, _) => !report_elements(name)
      case XML.Elem(Markup(name, _), _) => !report_elements(name)
      case _ => true
    } map {
      case XML.Wrapped_Elem(markup, body, ts) => XML.Wrapped_Elem(markup, body, clean_reports(ts))
      case XML.Elem(markup, ts) => XML.Elem(markup, clean_reports(ts))
      case t => t
    }

  def reports(props: Properties.T, body: XML.Body): List[XML.Elem] =
    body flatMap {
      case XML.Wrapped_Elem(Markup(Markup.REPORT, ps), body, ts) =>
        List(XML.Wrapped_Elem(Markup(Markup.REPORT, props ::: ps), body, ts))
      case XML.Elem(Markup(Markup.REPORT, ps), ts) =>
        List(XML.Elem(Markup(Markup.REPORT, props ::: ps), ts))
      case XML.Wrapped_Elem(_, _, ts) => reports(props, ts)
      case XML.Elem(_, ts) => reports(props, ts)
      case XML.Text(_) => Nil
    }


  /* reported positions */

  private val position_elements =
    Markup.Elements(Markup.BINDING, Markup.ENTITY, Markup.REPORT, Markup.POSITION)

  def positions(
    self_id: Document_ID.Generic => Boolean,
    command_position: Position.T,
    chunk_name: Symbol.Text_Chunk.Name,
    chunk: Symbol.Text_Chunk,
    message: XML.Elem): Set[Text.Range] =
  {
    def elem(props: Properties.T, set: Set[Text.Range]): Set[Text.Range] =
      props match {
        case Position.Identified(id, name) if self_id(id) && name == chunk_name =>
          val opt_range =
            Position.Range.unapply(props) orElse {
              if (name == Symbol.Text_Chunk.Default)
                Position.Range.unapply(command_position)
              else None
            }
          opt_range match {
            case Some(symbol_range) =>
              chunk.incorporate(symbol_range) match {
                case Some(range) => set + range
                case _ => set
              }
            case None => set
          }
        case _ => set
      }
    def tree(set: Set[Text.Range], t: XML.Tree): Set[Text.Range] =
      t match {
        case XML.Wrapped_Elem(Markup(name, props), _, body) =>
          body.foldLeft(if (position_elements(name)) elem(props, set) else set)(tree)
        case XML.Elem(Markup(name, props), body) =>
          body.foldLeft(if (position_elements(name)) elem(props, set) else set)(tree)
        case XML.Text(_) => set
      }

    val set = tree(Set.empty, message)
    if (set.isEmpty) elem(message.markup.properties, set)
    else set
  }
}
