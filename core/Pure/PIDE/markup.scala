/*  Title:      Pure/PIDE/markup.scala
    Module:     PIDE
    Author:     Makarius

Quasi-abstract markup elements.
*/

package isabelle


object Markup
{
  /* elements */

  object Elements
  {
    def apply(elems: Set[String]): Elements = new Elements(elems)
    def apply(elems: String*): Elements = apply(Set(elems: _*))
    val empty: Elements = apply()
    val full: Elements =
      new Elements(Set.empty)
      {
        override def apply(elem: String): Boolean = true
        override def toString: String = "Elements.full"
      }
  }

  sealed class Elements private[Markup](private val rep: Set[String])
  {
    def apply(elem: String): Boolean = rep.contains(elem)
    def + (elem: String): Elements = new Elements(rep + elem)
    def ++ (elems: Elements): Elements = new Elements(rep ++ elems.rep)
    override def toString: String = rep.mkString("Elements(", ",", ")")
  }


  /* properties */

  val NAME = "name"
  val Name = new Properties.String(NAME)

  val KIND = "kind"
  val Kind = new Properties.String(KIND)

  val INSTANCE = "instance"
  val Instance = new Properties.String(INSTANCE)


  /* basic markup */

  val Empty = Markup("", Nil)
  val Broken = Markup("broken", Nil)

  class Markup_String(val name: String, prop: String)
  {
    private val Prop = new Properties.String(prop)

    def apply(s: String): Markup = Markup(name, Prop(s))
    def unapply(markup: Markup): Option[String] =
      if (markup.name == name) Prop.unapply(markup.properties) else None
  }

  class Markup_Int(val name: String, prop: String)
  {
    private val Prop = new Properties.Int(prop)

    def apply(i: Int): Markup = Markup(name, Prop(i))
    def unapply(markup: Markup): Option[Int] =
      if (markup.name == name) Prop.unapply(markup.properties) else None
  }


  /* formal entities */

  val BINDING = "binding"
  val ENTITY = "entity"
  val DEF = "def"
  val REF = "ref"

  object Entity
  {
    def unapply(markup: Markup): Option[(String, String)] =
      markup match {
        case Markup(ENTITY, props) =>
          (props, props) match {
            case (Kind(kind), Name(name)) => Some((kind, name))
            case _ => None
          }
        case _ => None
      }
  }


  /* completion */

  val COMPLETION = "completion"
  val NO_COMPLETION = "no_completion"


  /* position */

  val LINE = "line"
  val END_LINE = "line"
  val OFFSET = "offset"
  val END_OFFSET = "end_offset"
  val FILE = "file"
  val ID = "id"

  val DEF_LINE = "def_line"
  val DEF_OFFSET = "def_offset"
  val DEF_END_OFFSET = "def_end_offset"
  val DEF_FILE = "def_file"
  val DEF_ID = "def_id"

  val POSITION_PROPERTIES = Set(LINE, OFFSET, END_OFFSET, FILE, ID)
  val POSITION = "position"


  /* expression */

  val EXPRESSION = "expression"


  /* citation */

  val CITATION = "citation"
  val Citation = new Markup_String(CITATION, NAME)


  /* embedded languages */

  val Symbols = new Properties.Boolean("symbols")
  val Antiquotes = new Properties.Boolean("antiquotes")
  val Delimited = new Properties.Boolean("delimited")

  val LANGUAGE = "language"
  object Language
  {
    val ML = "ML"
    val SML = "SML"
    val PATH = "path"
    val UNKNOWN = "unknown"

    def unapply(markup: Markup): Option[(String, Boolean, Boolean, Boolean)] =
      markup match {
        case Markup(LANGUAGE, props) =>
          (props, props, props, props) match {
            case (Name(name), Symbols(symbols), Antiquotes(antiquotes), Delimited(delimited)) =>
              Some((name, symbols, antiquotes, delimited))
            case _ => None
          }
        case _ => None
      }
  }


  /* external resources */

  val PATH = "path"
  val Path = new Markup_String(PATH, NAME)

  val URL = "url"
  val Url = new Markup_String(URL, NAME)


  /* pretty printing */

  val Block = new Markup_Int("block", "indent")
  val Break = new Markup_Int("break", "width")

  val ITEM = "item"
  val BULLET = "bullet"

  val SEPARATOR = "separator"


  /* text properties */

  val WORDS = "words"

  val HIDDEN = "hidden"


  /* misc entities */

  val CLASS = "class"
  val TYPE_NAME = "type_name"
  val FIXED = "fixed"
  val CASE = "case"
  val CONSTANT = "constant"
  val DYNAMIC_FACT = "dynamic_fact"


  /* inner syntax */

  val TFREE = "tfree"
  val TVAR = "tvar"
  val FREE = "free"
  val SKOLEM = "skolem"
  val BOUND = "bound"
  val VAR = "var"
  val NUMERAL = "numeral"
  val LITERAL = "literal"
  val DELIMITER = "delimiter"
  val INNER_STRING = "inner_string"
  val INNER_CARTOUCHE = "inner_cartouche"
  val INNER_COMMENT = "inner_comment"

  val TOKEN_RANGE = "token_range"

  val SORTING = "sorting"
  val TYPING = "typing"

  val ATTRIBUTE = "attribute"
  val METHOD = "method"


  /* antiquotations */

  val ANTIQUOTED = "antiquoted"
  val ANTIQUOTE = "antiquote"

  val ML_ANTIQUOTATION = "ML_antiquotation"
  val DOCUMENT_ANTIQUOTATION = "document_antiquotation"
  val DOCUMENT_ANTIQUOTATION_OPTION = "document_antiquotation_option"


  /* text structure */

  val PARAGRAPH = "paragraph"
  val TEXT_FOLD = "text_fold"


  /* ML syntax */

  val ML_KEYWORD1 = "ML_keyword1"
  val ML_KEYWORD2 = "ML_keyword2"
  val ML_KEYWORD3 = "ML_keyword3"
  val ML_DELIMITER = "ML_delimiter"
  val ML_TVAR = "ML_tvar"
  val ML_NUMERAL = "ML_numeral"
  val ML_CHAR = "ML_char"
  val ML_STRING = "ML_string"
  val ML_CARTOUCHE = "ML_cartouche"
  val ML_COMMENT = "ML_comment"
  val SML_STRING = "SML_string"
  val SML_COMMENT = "SML_comment"

  val ML_DEF = "ML_def"
  val ML_OPEN = "ML_open"
  val ML_STRUCTURE = "ML_structure"
  val ML_TYPING = "ML_typing"


  /* outer syntax */

  val COMMAND = "command"
  val KEYWORD1 = "keyword1"
  val KEYWORD2 = "keyword2"
  val KEYWORD3 = "keyword3"
  val QUASI_KEYWORD = "quasi_keyword"
  val IMPROPER = "improper"
  val OPERATOR = "operator"
  val STRING = "string"
  val ALT_STRING = "alt_string"
  val VERBATIM = "verbatim"
  val CARTOUCHE = "cartouche"
  val COMMENT = "comment"


  /* timing */

  val Elapsed = new Properties.Double("elapsed")
  val CPU = new Properties.Double("cpu")
  val GC = new Properties.Double("gc")

  object Timing_Properties
  {
    def apply(timing: isabelle.Timing): Properties.T =
      Elapsed(timing.elapsed.seconds) ::: CPU(timing.cpu.seconds) ::: GC(timing.gc.seconds)

    def unapply(props: Properties.T): Option[isabelle.Timing] =
      (props, props, props) match {
        case (Elapsed(elapsed), CPU(cpu), GC(gc)) =>
          Some(new isabelle.Timing(Time.seconds(elapsed), Time.seconds(cpu), Time.seconds(gc)))
        case _ => None
      }
  }

  val TIMING = "timing"

  object Timing
  {
    def apply(timing: isabelle.Timing): Markup = Markup(TIMING, Timing_Properties(timing))

    def unapply(markup: Markup): Option[isabelle.Timing] =
      markup match {
        case Markup(TIMING, Timing_Properties(timing)) => Some(timing)
        case _ => None
      }
  }


  /* command timing */

  val COMMAND_TIMING = "command_timing"


  /* toplevel */

  val SUBGOALS = "subgoals"
  val PROOF_STATE = "proof_state"

  val GOAL = "goal"
  val SUBGOAL = "subgoal"


  /* command status */

  val TASK = "task"

  val ACCEPTED = "accepted"
  val FORKED = "forked"
  val JOINED = "joined"
  val RUNNING = "running"
  val FINISHED = "finished"
  val FAILED = "failed"


  /* interactive documents */

  val VERSION = "version"
  val ASSIGN = "assign"


  /* prover process */

  val PROVER_COMMAND = "prover_command"
  val PROVER_ARG = "prover_arg"


  /* messages */

  val SERIAL = "serial"
  val Serial = new Properties.Long(SERIAL)

  val INIT = "init"
  val STATUS = "status"
  val REPORT = "report"
  val RESULT = "result"
  val WRITELN = "writeln"
  val STATE = "state"
  val INFORMATION = "information"
  val TRACING = "tracing"
  val WARNING = "warning"
  val LEGACY = "legacy"
  val ERROR = "error"
  val PROTOCOL = "protocol"
  val SYSTEM = "system"
  val STDOUT = "stdout"
  val STDERR = "stderr"
  val EXIT = "exit"

  val WRITELN_MESSAGE = "writeln_message"
  val STATE_MESSAGE = "state_message"
  val INFORMATION_MESSAGE = "information_message"
  val TRACING_MESSAGE = "tracing_message"
  val WARNING_MESSAGE = "warning_message"
  val LEGACY_MESSAGE = "legacy_message"
  val ERROR_MESSAGE = "error_message"

  val messages = Map(
    WRITELN -> WRITELN_MESSAGE,
    STATE -> STATE_MESSAGE,
    INFORMATION -> INFORMATION_MESSAGE,
    TRACING -> TRACING_MESSAGE,
    WARNING -> WARNING_MESSAGE,
    LEGACY -> LEGACY_MESSAGE,
    ERROR -> ERROR_MESSAGE)

  val message: String => String = messages.withDefault((s: String) => s)

  val Return_Code = new Properties.Int("return_code")

  val NO_REPORT = "no_report"

  val BAD = "bad"

  val INTENSIFY = "intensify"


  /* active areas */

  val BROWSER = "browser"
  val GRAPHVIEW = "graphview"

  val SENDBACK = "sendback"
  val PADDING = "padding"
  val PADDING_LINE = (PADDING, "line")
  val PADDING_COMMAND = (PADDING, "command")

  val DIALOG = "dialog"
  val Result = new Properties.String(RESULT)


  /* protocol message functions */

  val FUNCTION = "function"
  val Function = new Properties.String(FUNCTION)

  val Assign_Update: Properties.T = List((FUNCTION, "assign_update"))
  val Removed_Versions: Properties.T = List((FUNCTION, "removed_versions"))

  object Protocol_Handler
  {
    def unapply(props: Properties.T): Option[(String)] =
      props match {
        case List((FUNCTION, "protocol_handler"), (NAME, name)) => Some(name)
        case _ => None
      }
  }

  val INVOKE_SCALA = "invoke_scala"
  object Invoke_Scala
  {
    def unapply(props: Properties.T): Option[(String, String)] =
      props match {
        case List((FUNCTION, INVOKE_SCALA), (NAME, name), (ID, id)) => Some((name, id))
        case _ => None
      }
  }

  val CANCEL_SCALA = "cancel_scala"
  object Cancel_Scala
  {
    def unapply(props: Properties.T): Option[String] =
      props match {
        case List((FUNCTION, CANCEL_SCALA), (ID, id)) => Some(id)
        case _ => None
      }
  }

  object ML_Statistics
  {
    def unapply(props: Properties.T): Option[Properties.T] =
      props match {
        case (FUNCTION, "ML_statistics") :: stats => Some(stats)
        case _ => None
      }
  }

  object Task_Statistics
  {
    def unapply(props: Properties.T): Option[Properties.T] =
      props match {
        case (FUNCTION, "task_statistics") :: stats => Some(stats)
        case _ => None
      }
  }

  val LOADING_THEORY = "loading_theory"
  object Loading_Theory
  {
    def unapply(props: Properties.T): Option[String] =
      props match {
        case List((FUNCTION, LOADING_THEORY), (NAME, name)) => Some(name)
        case _ => None
      }
  }

  val BUILD_THEORIES_RESULT = "build_theories_result"
  object Build_Theories_Result
  {
    def unapply(props: Properties.T): Option[String] =
      props match {
        case List((FUNCTION, BUILD_THEORIES_RESULT), ("id", id)) => Some(id)
        case _ => None
      }
  }

  val PRINT_OPERATIONS = "print_operations"


  /* simplifier trace */

  val SIMP_TRACE_PANEL = "simp_trace_panel"

  val SIMP_TRACE_LOG = "simp_trace_log"
  val SIMP_TRACE_STEP = "simp_trace_step"
  val SIMP_TRACE_RECURSE = "simp_trace_recurse"
  val SIMP_TRACE_HINT = "simp_trace_hint"
  val SIMP_TRACE_IGNORE = "simp_trace_ignore"

  val SIMP_TRACE_CANCEL = "simp_trace_cancel"
  object Simp_Trace_Cancel
  {
    def unapply(props: Properties.T): Option[Long] =
      props match {
        case (FUNCTION, SIMP_TRACE_CANCEL) :: Serial(i) => Some(i)
        case _ => None
      }
  }
}


sealed case class Markup(name: String, properties: Properties.T)
{
  def markup(s: String): String =
    YXML.string_of_tree(XML.Elem(this, List(XML.Text(s))))
}
