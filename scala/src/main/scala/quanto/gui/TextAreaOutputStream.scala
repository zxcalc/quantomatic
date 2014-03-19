package quanto.gui

import java.io.OutputStream
import scala.swing.TextArea


class TextAreaOutputStream(textArea: TextArea) extends OutputStream {
  val sb = new StringBuilder

  override def flush() {
    textArea.append(sb.toString())
    sb.setLength(0)
  }

  override def write(b: Int) =
    b match {
      case '\r' => // do nothing
      case '\n' =>
        sb.append('\n')
        flush()
      case _ =>
        sb.append(b.toChar)
    }
}
