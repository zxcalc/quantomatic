/*  Title:      Pure/System/isabelle_process.scala
    Author:     Makarius
    Options:    :folding=explicit:collapseFolds=1:

Isabelle process management -- always reactive due to multi-threaded I/O.
*/

package isabelle


import java.io.{InputStream, OutputStream, BufferedOutputStream, IOException}


class Isabelle_Process(
  receiver: Prover.Message => Unit = Console.println(_),
  prover_args: List[String] = Nil)
{
  /* text and tree data */

  def encode(s: String): String = Symbol.encode(s)
  def decode(s: String): String = Symbol.decode(s)

  val xml_cache = new XML.Cache()


  /* output */

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

    val main = XML.Elem(Markup(kind, props), Protocol.clean_message(body))
    val reports = Protocol.message_reports(props, body)
    for (msg <- main :: reports) receiver(new Prover.Output(xml_cache.elem(msg)))
  }

  private def exit_message(rc: Int)
  {
    output(Markup.EXIT, Markup.Return_Code(rc), List(XML.Text("Return code: " + rc.toString)))
  }



  /** process manager **/

  def command_line(channel: System_Channel, args: List[String]): List[String] =
    Isabelle_System.getenv_strict("ISABELLE_PROCESS") :: (channel.isabelle_args ::: args)

  private val system_channel = System_Channel()

  private val process =
    try {
      val cmdline = command_line(system_channel, prover_args)
      new Isabelle_System.Managed_Process(null, null, false, cmdline: _*)
    }
    catch { case e: IOException => system_channel.accepted(); throw(e) }

  private val (_, process_result) =
    Simple_Thread.future("process_result") { process.join }

  private def terminate_process()
  {
    try { process.terminate }
    catch { case e: IOException => system_output("Failed to terminate Isabelle: " + e.getMessage) }
  }

  private val process_manager = Simple_Thread.fork("process_manager")
  {
    val (startup_failed, startup_errors) =
    {
      var finished: Option[Boolean] = None
      val result = new StringBuilder(100)
      while (finished.isEmpty && (process.stderr.ready || !process_result.is_finished)) {
        while (finished.isEmpty && process.stderr.ready) {
          try {
            val c = process.stderr.read
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

    process.stdin.close
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

  def interrupt()
  {
    try { process.interrupt }
    catch { case e: IOException => system_output("Failed to interrupt Isabelle: " + e.getMessage) }
  }

  def terminate()
  {
    command_input_close()
    system_output("Terminating Isabelle process")
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
      if (err) ("standard_error", process.stderr, Markup.STDERR)
      else ("standard_output", process.stdout, Markup.STDOUT)

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
          if (n <= default_buffer.size) default_buffer
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
