/*  Title:      Pure/PIDE/prover.scala
    Author:     Makarius
    Options:    :folding=explicit:

Prover process wrapping.
*/

package isabelle


import java.io.{InputStream, OutputStream, BufferedReader, BufferedOutputStream, IOException}


object Prover
{
  /* syntax */

  trait Syntax
  {
    def ++ (other: Syntax): Syntax
    def add_keywords(keywords: Thy_Header.Keywords): Syntax
    def parse_spans(input: CharSequence): List[Command_Span.Span]
    def load_command(name: String): Option[List[String]]
    def load_commands_in(text: String): Boolean
  }


  /* underlying system process */

  trait System_Process
  {
    def stdout: BufferedReader
    def stderr: BufferedReader
    def terminate: Unit
    def join: Int
  }


  /* messages */

  sealed abstract class Message

  class Input(val name: String, val args: List[String]) extends Message
  {
    override def toString: String =
      XML.Elem(Markup(Markup.PROVER_COMMAND, List((Markup.NAME, name))),
        args.map(s =>
          List(XML.Text("\n"), XML.elem(Markup.PROVER_ARG, YXML.parse_body(s)))).flatten).toString
  }

  class Output(val message: XML.Elem) extends Message
  {
    def kind: String = message.markup.name
    def properties: Properties.T = message.markup.properties
    def body: XML.Body = message.body

    def is_init = kind == Markup.INIT
    def is_exit = kind == Markup.EXIT
    def is_stdout = kind == Markup.STDOUT
    def is_stderr = kind == Markup.STDERR
    def is_system = kind == Markup.SYSTEM
    def is_status = kind == Markup.STATUS
    def is_report = kind == Markup.REPORT
    def is_syslog = is_init || is_exit || is_system || is_stderr

    override def toString: String =
    {
      val res =
        if (is_status || is_report) message.body.map(_.toString).mkString
        else Pretty.string_of(message.body)
      if (properties.isEmpty)
        kind.toString + " [[" + res + "]]"
      else
        kind.toString + " " +
          (for ((x, y) <- properties) yield x + "=" + y).mkString("{", ",", "}") + " [[" + res + "]]"
    }
  }

  class Protocol_Output(props: Properties.T, val bytes: Bytes)
    extends Output(XML.Elem(Markup(Markup.PROTOCOL, props), Nil))
  {
    lazy val text: String = bytes.toString
  }
}


abstract class Prover(
  receiver: Prover.Message => Unit,
  system_channel: System_Channel,
  system_process: Prover.System_Process) extends Protocol
{
  /** receiver output **/

  val xml_cache: XML.Cache = new XML.Cache()

  private def system_output(text: String)
  {
    receiver(new Prover.Output(XML.Elem(Markup(Markup.SYSTEM, Nil), List(XML.Text(text)))))
  }

  private def protocol_output(props: Properties.T, bytes: Bytes)
  {
    receiver(new Prover.Protocol_Output(props, bytes))
  }

  private def output(kind: String, props: Properties.T, body: XML.Body)
  {
    if (kind == Markup.INIT) system_channel.accepted()

    val main = XML.Elem(Markup(kind, props), Protocol_Message.clean_reports(body))
    val reports = Protocol_Message.reports(props, body)
    for (msg <- main :: reports) receiver(new Prover.Output(xml_cache.elem(msg)))
  }

  private def exit_message(rc: Int)
  {
    output(Markup.EXIT, Markup.Return_Code(rc), List(XML.Text("Return code: " + rc.toString)))
  }



  /** process manager **/

  private val (_, process_result) =
    Simple_Thread.future("process_result") { system_process.join }

  private def terminate_process()
  {
    try { system_process.terminate }
    catch {
      case exn @ ERROR(_) => system_output("Failed to terminate prover process: " + exn.getMessage)
    }
  }

  private val process_manager = Simple_Thread.fork("process_manager")
  {
    val (startup_failed, startup_errors) =
    {
      var finished: Option[Boolean] = None
      val result = new StringBuilder(100)
      while (finished.isEmpty && (system_process.stderr.ready || !process_result.is_finished)) {
        while (finished.isEmpty && system_process.stderr.ready) {
          try {
            val c = system_process.stderr.read
            if (c == 2) finished = Some(true)
            else result += c.toChar
          }
          catch { case _: IOException => finished = Some(false) }
        }
        Thread.sleep(10)
      }
      (finished.isEmpty || !finished.get, result.toString.trim)
    }
    if (startup_errors != "") system_output(startup_errors)

    if (startup_failed) {
      terminate_process()
      process_result.join
      exit_message(127)
    }
    else {
      val (command_stream, message_stream) = system_channel.rendezvous()

      command_input_init(command_stream)
      val stdout = physical_output(false)
      val stderr = physical_output(true)
      val message = message_output(message_stream)

      val rc = process_result.join
      system_output("process terminated")
      command_input_close()
      for (thread <- List(stdout, stderr, message)) thread.join
      system_output("process_manager terminated")
      exit_message(rc)
    }
    system_channel.accepted()
  }


  /* management methods */

  def join() { process_manager.join() }

  def terminate()
  {
    command_input_close()
    system_output("Terminating prover process")
    terminate_process()
  }



  /** process streams **/

  /* command input */

  private var command_input: Option[Consumer_Thread[List[Bytes]]] = None

  private def command_input_close(): Unit = command_input.foreach(_.shutdown)

  private def command_input_init(raw_stream: OutputStream)
  {
    val name = "command_input"
    val stream = new BufferedOutputStream(raw_stream)
    command_input =
      Some(
        Consumer_Thread.fork(name)(
          consume =
            {
              case chunks =>
                try {
                  Bytes(chunks.map(_.length).mkString("", ",", "\n")).write(stream)
                  chunks.foreach(_.write(stream))
                  stream.flush
                  true
                }
                catch { case e: IOException => system_output(name + ": " + e.getMessage); false }
            },
          finish = { case () => stream.close; system_output(name + " terminated") }
        )
      )
  }


  /* physical output */

  private def physical_output(err: Boolean): Thread =
  {
    val (name, reader, markup) =
      if (err) ("standard_error", system_process.stderr, Markup.STDERR)
      else ("standard_output", system_process.stdout, Markup.STDOUT)

    Simple_Thread.fork(name) {
      try {
        var result = new StringBuilder(100)
        var finished = false
        while (!finished) {
          //{{{
          var c = -1
          var done = false
          while (!done && (result.length == 0 || reader.ready)) {
            c = reader.read
            if (c >= 0) result.append(c.asInstanceOf[Char])
            else done = true
          }
          if (result.length > 0) {
            output(markup, Nil, List(XML.Text(decode(result.toString))))
            result.length = 0
          }
          else {
            reader.close
            finished = true
          }
          //}}}
        }
      }
      catch { case e: IOException => system_output(name + ": " + e.getMessage) }
      system_output(name + " terminated")
    }
  }


  /* message output */

  private def message_output(stream: InputStream): Thread =
  {
    class EOF extends Exception
    class Protocol_Error(msg: String) extends Exception(msg)

    val name = "message_output"
    Simple_Thread.fork(name) {
      val default_buffer = new Array[Byte](65536)
      var c = -1

      def read_int(): Int =
      //{{{
      {
        var n = 0
        c = stream.read
        if (c == -1) throw new EOF
        while (48 <= c && c <= 57) {
          n = 10 * n + (c - 48)
          c = stream.read
        }
        if (c != 10)
          throw new Protocol_Error("malformed header: expected integer followed by newline")
        else n
      }
      //}}}

      def read_chunk_bytes(): (Array[Byte], Int) =
      //{{{
      {
        val n = read_int()
        val buf =
          if (n <= default_buffer.length) default_buffer
          else new Array[Byte](n)

        var i = 0
        var m = 0
        do {
          m = stream.read(buf, i, n - i)
          if (m != -1) i += m
        }
        while (m != -1 && n > i)

        if (i != n)
          throw new Protocol_Error("bad chunk (unexpected EOF after " + i + " of " + n + " bytes)")

        (buf, n)
      }
      //}}}

      def read_chunk(): XML.Body =
      {
        val (buf, n) = read_chunk_bytes()
        YXML.parse_body_failsafe(UTF8.decode_chars(decode, buf, 0, n))
      }

      try {
        do {
          try {
            val header = read_chunk()
            header match {
              case List(XML.Elem(Markup(name, props), Nil)) =>
                val kind = name.intern
                if (kind == Markup.PROTOCOL) {
                  val (buf, n) = read_chunk_bytes()
                  protocol_output(props, Bytes(buf, 0, n))
                }
                else {
                  val body = read_chunk()
                  output(kind, props, body)
                }
              case _ =>
                read_chunk()
                throw new Protocol_Error("bad header: " + header.toString)
            }
          }
          catch { case _: EOF => }
        }
        while (c != -1)
      }
      catch {
        case e: IOException => system_output("Cannot read message:\n" + e.getMessage)
        case e: Protocol_Error => system_output("Malformed message:\n" + e.getMessage)
      }
      stream.close

      system_output(name + " terminated")
    }
  }



  /** protocol commands **/

  def protocol_command_bytes(name: String, args: Bytes*): Unit =
    command_input match {
      case Some(thread) => thread.send(Bytes(name) :: args.toList)
      case None => error("Uninitialized command input thread")
    }

  def protocol_command(name: String, args: String*)
  {
    receiver(new Prover.Input(name, args.toList))
    protocol_command_bytes(name, args.map(Bytes(_)): _*)
  }
}
