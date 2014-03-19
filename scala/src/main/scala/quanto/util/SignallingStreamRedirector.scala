package quanto.util

import java.io.{IOException, OutputStream, InputStream}
import java.util.logging.Level

sealed abstract class Signal(id: Int)
case class Success(id: Int) extends Signal(id)
case class Failed(id: Int) extends Signal(id)
case class Interrupted(id: Int) extends Signal(id)

/**
 * A simple stream redirector that allows bits of code to listen for signals coming over the stream, all of
 * this form: <<[S](id)>>, <<[F](id)>>, <<[I](id)>> indicating success, failure, and interrupt (along with
 * some identifying code).
 *
 */

class SignallingStreamRedirector(from: InputStream, to: OutputStream)
extends Thread("Signalling Stream Redirector") {
  private var state = 0
  private var currentCode = 'X'
  private var currentId = 0
  private val listeners = collection.mutable.Map[Int, List[(Signal => Any)]]()

  private def fire(code: Char, id: Int) {
    val sig = code match {
      case 'S' => Success(id)
      case 'F' => Failed(id)
      case 'I' => Interrupted(id)
    }

    listeners(id).foreach { f => f(sig) }
    listeners.remove(id)
  }

  // process string via tiny state machine
  private def processChar(c: Char) = state match {
    case 0 => if (c == '<') state = 1
    case 1 => if (c == '<') state = 2 else state = 0
    case 2 => if (c == '[') state = 3 else state = 0
    case 3 => if (c == 'S' || c == 'F' || c == 'I') { currentCode = c ; state = 4 }
    else { currentCode = 'X' ; state = 0 }
    case 4 => if (c == ']') state = 5
    else { currentCode = 'X' ; state = 0 }
    case 5 => if (c.isDigit) currentId = (10 * currentId) + c.toInt
    else if (c == '>') state = 6
    else { currentCode = 'X' ; currentId = 0; state = 0 }
    case 6 => if (c == '>') fire(currentCode, currentId)
      currentCode = 'X' ; currentId = 0; state = 0
  }

  def addListener(id: Int)(f : Signal => Any) {
    listeners.put(id, listeners.get(id) match {
      case Some(list) => f :: list
      case None => List(f)
    })
  }

  override def run() {
    try {
      val buffer: Array[Byte] = new Array[Byte](200)
      var count: Int = from.read(buffer)
      while (count != -1) {
        for (i <- 0 to count - 1) processChar(buffer(i).toChar)
        to.write(buffer, 0, count)
        to.flush()
        count = from.read(buffer)
      }
    }
    catch {
      case ex: IOException =>
        ex.printStackTrace()
    }
  }
}
