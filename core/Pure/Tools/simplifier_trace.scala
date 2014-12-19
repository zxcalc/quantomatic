/*  Title:      Pure/Tools/simplifier_trace.scala
    Author:     Lars Hupel

Interactive Simplifier trace.
*/

package isabelle


import scala.annotation.tailrec
import scala.collection.immutable.SortedMap


object Simplifier_Trace
{
  /* trace items from the prover */

  val TEXT = "text"
  val Text = new Properties.String(TEXT)

  val PARENT = "parent"
  val Parent = new Properties.Long(PARENT)

  val SUCCESS = "success"
  val Success = new Properties.Boolean(SUCCESS)

  val MEMORY = "memory"
  val Memory = new Properties.Boolean(MEMORY)

  object Item
  {
    case class Data(
      serial: Long, markup: String, text: String,
      parent: Long, props: Properties.T, content: XML.Body)
    {
      def memory: Boolean = Memory.unapply(props) getOrElse true
    }

    def unapply(tree: XML.Tree): Option[(String, Data)] =
      tree match {
        case XML.Elem(Markup(Markup.RESULT, Markup.Serial(serial)),
          List(XML.Elem(Markup(markup, props), content)))
        if markup.startsWith("simp_trace_") =>  // FIXME proper comparison of string constants
          (props, props) match {
            case (Text(text), Parent(parent)) =>
              Some((markup, Data(serial, markup, text, parent, props, content)))
            case _ => None
          }
        case _ => None
      }
  }


  /* replies to the prover */

  case class Answer private[Simplifier_Trace](val name: String, val string: String)

  object Answer
  {
    object step
    {
      val skip = Answer("skip", "Skip")
      val continue = Answer("continue", "Continue")
      val continue_trace = Answer("continue_trace", "Continue (with full trace)")
      val continue_passive = Answer("continue_passive", "Continue (without asking)")
      val continue_disable = Answer("continue_disable", "Continue (without any trace)")

      val all = List(continue, continue_trace, continue_passive, continue_disable, skip)
    }

    object hint_fail
    {
      val exit = Answer("exit", "Exit")
      val redo = Answer("redo", "Redo")

      val all = List(redo, exit)
    }
  }

  val all_answers: List[Answer] = Answer.step.all ::: Answer.hint_fail.all


  /* GUI interaction */

  case object Event


  /* manager thread */

  private case class Handle_Results(
    session: Session, id: Document_ID.Command, results: Command.Results, slot: Promise[Context])
  private case class Generate_Trace(results: Command.Results, slot: Promise[Trace])
  private case class Cancel(serial: Long)
  private object Clear_Memory
  case class Reply(session: Session, serial: Long, answer: Answer)

  case class Question(data: Item.Data, answers: List[Answer])

  case class Context(
    last_serial: Long = 0L,
    questions: SortedMap[Long, Question] = SortedMap.empty)
  {
    def +(q: Question): Context =
      copy(questions = questions + ((q.data.serial, q)))

    def -(s: Long): Context =
      copy(questions = questions - s)

    def with_serial(s: Long): Context =
      copy(last_serial = Math.max(last_serial, s))
  }

  case class Trace(entries: List[Item.Data])

  case class Index(text: String, content: XML.Body)

  object Index
  {
    def of_data(data: Item.Data): Index =
      Index(data.text, data.content)
  }

  def handle_results(session: Session, id: Document_ID.Command, results: Command.Results): Context =
  {
    val slot = Future.promise[Context]
    manager.send(Handle_Results(session, id, results, slot))
    slot.join
  }

  def generate_trace(results: Command.Results): Trace =
  {
    val slot = Future.promise[Trace]
    manager.send(Generate_Trace(results, slot))
    slot.join
  }

  def clear_memory() =
    manager.send(Clear_Memory)

  def send_reply(session: Session, serial: Long, answer: Answer) =
    manager.send(Reply(session, serial, answer))

  private lazy val manager: Consumer_Thread[Any] =
  {
    var contexts = Map.empty[Document_ID.Command, Context]

    var memory_children = Map.empty[Long, Set[Long]]
    var memory = Map.empty[Index, Answer]

    def find_question(serial: Long): Option[(Document_ID.Command, Question)] =
      contexts collectFirst {
        case (id, context) if context.questions contains serial =>
          (id, context.questions(serial))
      }

    def do_cancel(serial: Long, id: Document_ID.Command)
    {
      // To save memory, we could try to remove empty contexts at this point.
      // However, if a new serial gets attached to the same command_id after we deleted
      // its context, its last_serial counter will start at 0 again, and we'll think the
      // old serials are actually new
      contexts += (id -> (contexts(id) - serial))
    }

    def do_reply(session: Session, serial: Long, answer: Answer)
    {
      session.protocol_command(
        "Simplifier_Trace.reply", Properties.Value.Long(serial), answer.name)
    }

    Consumer_Thread.fork[Any]("Simplifier_Trace.manager", daemon = true)(
      consume = (arg: Any) =>
      {
        arg match {
          case Handle_Results(session, id, results, slot) =>
            var new_context = contexts.getOrElse(id, Context())
            var new_serial = new_context.last_serial

            for ((serial, result) <- results.iterator if serial > new_context.last_serial)
            {
              result match {
                case Item(markup, data) =>
                  memory_children +=
                    (data.parent -> (memory_children.getOrElse(data.parent, Set.empty) + serial))

                  markup match {

                    case Markup.SIMP_TRACE_STEP =>
                      val index = Index.of_data(data)
                      memory.get(index) match {
                        case Some(answer) if data.memory =>
                          do_reply(session, serial, answer)
                        case _ =>
                          new_context += Question(data, Answer.step.all)
                      }

                    case Markup.SIMP_TRACE_HINT =>
                      data.props match {
                        case Success(false) =>
                          results.get(data.parent) match {
                            case Some(Item(Markup.SIMP_TRACE_STEP, _)) =>
                              new_context += Question(data, Answer.hint_fail.all)
                            case _ =>
                              // unknown, better send a default reply
                              do_reply(session, data.serial, Answer.hint_fail.exit)
                          }
                        case _ =>
                      }

                    case Markup.SIMP_TRACE_IGNORE =>
                      // At this point, we know that the parent of this 'IGNORE' entry is a 'STEP'
                      // entry, and that that 'STEP' entry is about to be replayed. Hence, we need
                      // to selectively purge the replies which have been memorized, going down from
                      // the parent to all leaves.

                      @tailrec
                      def purge(queue: Vector[Long]): Unit =
                        queue match {
                          case s +: rest =>
                            for (Item(Markup.SIMP_TRACE_STEP, data) <- results.get(s))
                              memory -= Index.of_data(data)
                            val children = memory_children.getOrElse(s, Set.empty)
                            memory_children -= s
                            purge(rest ++ children.toVector)
                          case _ =>
                        }

                      purge(Vector(data.parent))

                    case _ =>
                  }

                case _ =>
              }

              new_serial = serial
            }

            new_context = new_context.with_serial(new_serial)
            contexts += (id -> new_context)
            slot.fulfill(new_context)

          case Generate_Trace(results, slot) =>
            // Since there are potentially lots of trace messages, we do not cache them here again.
            // Instead, everytime the trace is being requested, we re-assemble it based on the
            // current results.

            val items =
              results.iterator.collect { case (_, Item(_, data)) => data }.toList

            slot.fulfill(Trace(items))

          case Cancel(serial) =>
            find_question(serial) match {
              case Some((id, _)) =>
                do_cancel(serial, id)
              case None =>
            }

          case Clear_Memory =>
            memory_children = Map.empty
            memory = Map.empty

          case Reply(session, serial, answer) =>
            find_question(serial) match {
              case Some((id, Question(data, _))) =>
                if (data.markup == Markup.SIMP_TRACE_STEP && data.memory)
                {
                  val index = Index.of_data(data)
                  memory += (index -> answer)
                }
                do_cancel(serial, id)
              case None =>
                Output.warning("send_reply: unknown serial " + serial)
            }

            do_reply(session, serial, answer)
            session.trace_events.post(Event)
        }
        true
      },
      finish = () => contexts = Map.empty
    )
  }


  /* protocol handler */

  class Handler extends Session.Protocol_Handler
  {
    assert(manager.is_active)

    private def cancel(prover: Prover, msg: Prover.Protocol_Output): Boolean =
      msg.properties match {
        case Markup.Simp_Trace_Cancel(serial) =>
          manager.send(Cancel(serial))
          true
        case _ =>
          false
      }

    override def stop(prover: Prover) =
    {
      manager.send(Clear_Memory)
      manager.shutdown()
    }

    val functions = Map(Markup.SIMP_TRACE_CANCEL -> cancel _)
  }
}
