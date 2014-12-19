/*  Title:      Pure/System/isabelle_font.scala
    Author:     Makarius

Isabelle font support.
*/

package isabelle


import java.awt.{GraphicsEnvironment, Font}
import java.io.{FileInputStream, BufferedInputStream}
import javafx.scene.text.{Font => JFX_Font}


object Isabelle_Font
{
  def apply(family: String = "IsabelleText", size: Int = 1, bold: Boolean = false): Font =
    new Font(family, if (bold) Font.BOLD else Font.PLAIN, size)

  def install_fonts()
  {
    val ge = GraphicsEnvironment.getLocalGraphicsEnvironment()
    for (font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS")))
      ge.registerFont(Font.createFont(Font.TRUETYPE_FONT, font.file))
  }

  def install_fonts_jfx()
  {
    for (font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS"))) {
      val stream = new BufferedInputStream(new FileInputStream(font.file))
      try { JFX_Font.loadFont(stream, 1.0) }
      finally { stream.close }
    }
  }
}

