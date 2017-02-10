/*  Title:      Pure/Tools/debugger.scala
    Author:     Makarius

Interactive debugger for Isabelle/ML.
*/

package isabelle


import scala.collection.immutable.SortedMap


object Debugger
{
  /* context */

  sealed case class Debug_State(
    pos: Position.T,
    function: String)

  sealed case class Context(thread_name: String, debug_states: List[Debug_State], index: Int = 0)
  {
    def size: Int = debug_states.length + 1
    def reset: Context = copy(index = 0)
    def select(i: Int) = copy(index = i + 1)

    def thread_state: Option[Debug_State] = debug_states.headOption

    def stack_state: Option[Debug_State] =
      if (1 <= index && index <= debug_states.length)
        Some(debug_states(index - 1))
      else None

    def debug_index: Option[Int] =
      if (stack_state.isDefined) Some(index - 1)
      else if (debug_states.nonEmpty) Some(0)
      else None

    def debug_state: Option[Debug_State] = stack_state orElse thread_state
    def debug_position: Option[Position.T] = debug_state.map(_.pos)

    override def toString: String =
      stack_state match {
        case None => thread_name
        case Some(d) => d.function
      }
  }


  /* global state */

  type Threads = SortedMap[String, List[Debug_State]]

  sealed case class State(
    session: Session = new Session(Resources.empty),  // implicit session
    active: Int = 0,  // active views
    break: Boolean = false,  // break at next possible breakpoint
    active_breakpoints: Set[Long] = Set.empty,  // explicit breakpoint state
    threads: Threads = SortedMap.empty,  // thread name ~> stack of debug states
    focus: Map[String, Context] = Map.empty,  // thread name ~> focus
    output: Map[String, Command.Results] = Map.empty)  // thread name ~> output messages
  {
    def set_session(new_session: Session): State =
    {
      session.stop()
      copy(session = new_session)
    }

    def set_break(b: Boolean): State = copy(break = b)

    def is_active: Boolean = active > 0 && session.is_ready
    def inc_active: State = copy(active = active + 1)
    def dec_active: State = copy(active = active - 1)

    def toggle_breakpoint(breakpoint: Long): (Boolean, State) =
    {
      val active_breakpoints1 =
        if (active_breakpoints(breakpoint)) active_breakpoints - breakpoint
      else active_breakpoints + breakpoint
      (active_breakpoints1(breakpoint), copy(active_breakpoints = active_breakpoints1))
    }

    def get_thread(thread_name: String): List[Debug_State] =
      threads.getOrElse(thread_name, Nil)

    def update_thread(thread_name: String, debug_states: List[Debug_State]): State =
    {
      val threads1 =
        if (debug_states.nonEmpty) threads + (thread_name -> debug_states)
        else threads - thread_name
      val focus1 =
        focus.get(thread_name) match {
          case Some(c) if debug_states.nonEmpty =>
            focus + (thread_name -> Context(thread_name, debug_states))
          case _ => focus - thread_name
        }
      copy(threads = threads1, focus = focus1)
    }

    def set_focus(c: Context): State = copy(focus = focus + (c.thread_name -> c))

    def get_output(thread_name: String): Command.Results =
      output.getOrElse(thread_name, Command.Results.empty)

    def add_output(thread_name: String, entry: Command.Results.Entry): State =
      copy(output = output + (thread_name -> (get_output(thread_name) + entry)))

    def clear_output(thread_name: String): State =
      copy(output = output - thread_name)
  }

  private val global_state = Synchronized(State())


  /* update events */

  case object Update

  private val delay_update =
    Standard_Thread.delay_first(global_state.value.session.output_delay) {
      global_state.value.session.debugger_updates.post(Update)
    }


  /* protocol handler */

  class Handler extends Session.Protocol_Handler
  {
    private def debugger_state(prover: Prover, msg: Prover.Protocol_Output): Boolean =
    {
      msg.properties match {
        case Markup.Debugger_State(thread_name) =>
          val msg_body =
            YXML.parse_body_failsafe(Symbol.decode(UTF8.decode_permissive(msg.bytes)))
          val debug_states =
          {
            import XML.Decode._
            list(pair(properties, Symbol.decode_string))(msg_body).map({
              case (pos, function) => Debug_State(pos, function)
            })
          }
          global_state.change(_.update_thread(thread_name, debug_states))
          delay_update.invoke()
          true
        case _ => false
      }
    }

    private def debugger_output(prover: Prover, msg: Prover.Protocol_Output): Boolean =
    {
      msg.properties match {
        case Markup.Debugger_Output(thread_name) =>
          val msg_body =
            prover.xml_cache.body(
              YXML.parse_body_failsafe(Symbol.decode(UTF8.decode_permissive(msg.bytes))))
          msg_body match {
            case List(XML.Elem(Markup(name, props @ Markup.Serial(i)), body)) =>
              val message = XML.Elem(Markup(Markup.message(name), props), body)
              global_state.change(_.add_output(thread_name, i -> message))
              delay_update.invoke()
              true
            case _ => false
          }
        case _ => false
      }
    }

    val functions =
      Map(
        Markup.DEBUGGER_STATE -> debugger_state _,
        Markup.DEBUGGER_OUTPUT -> debugger_output _)
  }


  /* protocol commands */

  def is_active(): Boolean = global_state.value.is_active

  def init_session(session: Session)
  {
    global_state.change(state =>
    {
      val state1 = state.set_session(session)
      if (!state.session.is_ready && state1.session.is_ready && state1.is_active)
        state1.session.protocol_command("Debugger.init")
      state1
    })
  }

  def init(): Unit =
    global_state.change(state =>
    {
      val state1 = state.inc_active
      if (!state.is_active && state1.is_active)
        state1.session.protocol_command("Debugger.init")
      state1
    })

  def exit(): Unit =
    global_state.change(state =>
    {
      val state1 = state.dec_active
      if (state.is_active && !state1.is_active)
        state1.session.protocol_command("Debugger.exit")
      state1
    })

  def is_break(): Boolean = global_state.value.break
  def set_break(b: Boolean)
  {
    global_state.change(state => {
      val state1 = state.set_break(b)
      state1.session.protocol_command("Debugger.break", b.toString)
      state1
    })
    delay_update.invoke()
  }

  def active_breakpoint_state(breakpoint: Long): Option[Boolean] =
  {
    val state = global_state.value
    if (state.is_active) Some(state.active_breakpoints(breakpoint)) else None
  }

  def breakpoint_state(breakpoint: Long): Boolean =
    global_state.value.active_breakpoints(breakpoint)

  def toggle_breakpoint(command: Command, breakpoint: Long)
  {
    global_state.change(state =>
    {
      val (breakpoint_state, state1) = state.toggle_breakpoint(breakpoint)
      state1.session.protocol_command(
        "Debugger.breakpoint",
        Symbol.encode(command.node_name.node),
        Document_ID(command.id),
        Value.Long(breakpoint),
        Value.Boolean(breakpoint_state))
      state1
    })
  }

  def status(focus: Option[Context]): (Threads, List[XML.Tree]) =
  {
    val state = global_state.value
    val output =
      focus match {
        case None => Nil
        case Some(c) =>
          (for {
            (thread_name, results) <- state.output
            if thread_name == c.thread_name
            (_, tree) <- results.iterator
          } yield tree).toList
      }
    (state.threads, output)
  }

  def focus(): List[Context] = global_state.value.focus.toList.map(_._2)
  def set_focus(c: Context)
  {
    global_state.change(_.set_focus(c))
    delay_update.invoke()
  }

  def input(thread_name: String, msg: String*): Unit =
    global_state.value.session.protocol_command("Debugger.input", (thread_name :: msg.toList):_*)

  def continue(thread_name: String): Unit = input(thread_name, "continue")
  def step(thread_name: String): Unit = input(thread_name, "step")
  def step_over(thread_name: String): Unit = input(thread_name, "step_over")
  def step_out(thread_name: String): Unit = input(thread_name, "step_out")

  def clear_output(thread_name: String)
  {
    global_state.change(_.clear_output(thread_name))
    delay_update.invoke()
  }

  def eval(c: Context, SML: Boolean, context: String, expression: String)
  {
    global_state.change(state => {
      input(c.thread_name, "eval", c.debug_index.getOrElse(0).toString,
        SML.toString, Symbol.encode(context), Symbol.encode(expression))
      state.clear_output(c.thread_name)
    })
    delay_update.invoke()
  }

  def print_vals(c: Context, SML: Boolean, context: String)
  {
    require(c.debug_index.isDefined)

    global_state.change(state => {
      input(c.thread_name, "print_vals", c.debug_index.getOrElse(0).toString,
        SML.toString, Symbol.encode(context))
      state.clear_output(c.thread_name)
    })
    delay_update.invoke()
  }
}
