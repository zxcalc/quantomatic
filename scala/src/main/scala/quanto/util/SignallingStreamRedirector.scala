package quanto.util

import java.io._

//sealed abstract class Signal(id: Int)
//case class SuccessSignal(id: Int) extends Signal(id)
//case class FailedSignal(id: Int) extends Signal(id)
//case class InterruptedSignal(id: Int) extends Signal(id)

sealed abstract class MessagePart
case class CodePart(c: Char) extends MessagePart
case class IntPart(i: Int) extends MessagePart
case class StringPart(s: String) extends MessagePart

case class StreamMessage(parts: MessagePart*) {

  def writeTo(out: Writer) {
    parts.foreach {
      case CodePart(c: Char) =>
        out.write('\u001B')
        out.write(c)
      case IntPart(i: Int) =>
        out.write(i.toString)
      case StringPart(s: String) =>
        out.write(s)
    }

    out.flush()
  }

  def writeTo(out: OutputStream) {
    writeTo(new OutputStreamWriter(out))
  }

  def stripCodes = parts.filter{ case _: CodePart => false ; case _ => true }
}

object StreamMessage {
  def compileMessage(id: Int, fileName: String, code: String) = {
    new StreamMessage(
      CodePart('R'),
      IntPart(id), CodePart(','),
      StringPart(fileName), CodePart(','), // sourcename
      IntPart(0), CodePart(','), // startposition
      IntPart(0), CodePart(','), // prelude-length
      IntPart(code.length), CodePart(','), // source-length
      StringPart(""), CodePart(','), // prelude
      StringPart(code), // source-text
      CodePart('r')
    )
  }
}

/**
 * A simple stream redirector that allows bits of code to listen for signals coming over the stream, all of
 * this form: <<[S](id)>>, <<[F](id)>>, <<[I](id)>> indicating success, failure, and interrupt (along with
 * some identifying code).
 *
 */

class SignallingStreamRedirector(from: InputStream, to: Option[OutputStream] = None)
extends Thread("Signalling Stream Redirector") {
  private var state = 0
  private var currentCode: Option[Char] = None
  private var currentId = 0
  private var buf = new StringBuffer
  private var msgParts = Seq[MessagePart]()
  private val listeners = collection.mutable.Map[Int, List[(StreamMessage => Any)]]()
  private val outputStreams = collection.mutable.Buffer[OutputStream]()
  to.map { s => outputStreams += s }

  private def fire() {
    msgParts match {
      case (_ :: IntPart(id) :: _) =>
        val msg = StreamMessage(msgParts: _*)
        listeners.synchronized {
          listeners.remove(id).map { _.reverse.foreach( f => f(msg)) }
        }
      case _ =>
        println("Got bad message: " + msgParts)
    }
  }

  // process string via tiny state machine
//  private def processChar(c: Char) = state match {
//    case 0 => if (c == '<') state = 1
//    case 1 => if (c == '<') state = 2 else state = 0
//    case 2 => if (c == '[') state = 3 else state = 0
//    case 3 => if (c == 'S' || c == 'F' || c == 'I') { currentCode = c ; state = 4 }
//              else { currentCode = 'X' ; state = 0 }
//    case 4 => if (c == ']') state = 5
//              else { currentCode = 'X' ; state = 0 }
//    case 5 => if (c.isDigit) currentId = (10 * currentId) + c.toString.toInt
//              else if (c == '>') state = 6
//              else { currentCode = 'X' ; currentId = 0; state = 0 }
//    case 6 => if (c == '>') fire(currentCode, currentId)
//              currentCode = 'X' ; currentId = 0; state = 0
//  }

  private def resetState() {
    msgParts = List()
    buf = new StringBuffer
    currentCode = None
    currentId = 0
    state = 0
  }

  private def processChar(c: Char) =
    state match {
      case 0 =>
        if (c == '\u001B') { state = 1 ; false }
        else { state = 0; true }
      case 1 =>
        msgParts :+= CodePart(c)
        currentCode = Some(c.toLower)
        state = 2
        false
      case 2 =>
        if (c.isDigit) { currentId = (10 * currentId) + c.toString.toInt }
        else if (c == '\u001B') { msgParts :+= IntPart(currentId); state = 3 }
        else { resetState() }
        false
      case 3 => msgParts :+= CodePart(c)
        if (Some(c) == currentCode) { fire(); resetState() }
        else { state = 4 }
        false
      case 4 =>
        if (c == '\u001B') {  msgParts :+= StringPart(buf.toString); buf = new StringBuffer; state = 3 }
        else { buf.append(c) }
        false
    }

  def addListener(id: Int)(f : StreamMessage => Any) {
    listeners.synchronized {
      listeners.put(id, listeners.get(id) match {
        case Some(list) => f :: list
        case None => List(f)
      })
    }
  }

  def addOutputStream(out: OutputStream) {
    outputStreams.synchronized {
      outputStreams += out
    }
  }

  def removeOutputStream(out : OutputStream) {
    outputStreams.synchronized {
      outputStreams -= out
    }
  }

  override def run() {
    try {
      val buffer: Array[Byte] = new Array[Byte](200)
      val buffer1 : Array[Byte] = new Array[Byte](200)
      var count: Int = from.read(buffer)

      while (count != -1) {
        var j = 0
        for (i <- 0 to count - 1) {
          if (processChar(buffer(i).toChar)) {
            buffer1(j) = buffer(i)
            j += 1
          }
        }

        outputStreams.synchronized {
          outputStreams.foreach { out =>
            out.write(buffer1, 0, j)
            out.flush()
          }
        }

        count = from.read(buffer)
      }
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }
}
