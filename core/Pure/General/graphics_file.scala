/*  Title:      Pure/General/graphics_file.scala
    Author:     Makarius

File system operations for Graphics2D output.
*/

package isabelle


import java.io.{FileOutputStream, BufferedOutputStream, File => JFile}
import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import org.jfree.chart.JFreeChart

import com.lowagie.text.pdf.{PdfWriter, BaseFont, FontMapper, DefaultFontMapper}


object Graphics_File
{
  /* PNG */

  def write_png(file: JFile, paint: Graphics2D => Unit, width: Int, height: Int, dpi: Int = 72)
  {
    val scale = dpi / 72.0f
    val w = (width * scale).round
    val h = (height * scale).round

    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val gfx = img.createGraphics
    try {
      gfx.scale(scale, scale)
      paint(gfx)
      ImageIO.write(img, "png", file)
    }
    finally { gfx.dispose }
  }


  /* PDF */

  private def font_mapper(): FontMapper =
  {
    val mapper = new DefaultFontMapper
    for {
      font <- Path.split(Isabelle_System.getenv_strict("ISABELLE_FONTS"))
      name <- Library.try_unsuffix(".ttf", font.base.implode)
    } {
      val parameters = new DefaultFontMapper.BaseFontParameters(File.platform_path(font))
      parameters.encoding = BaseFont.IDENTITY_H
      mapper.putName(name, parameters)
    }
    mapper
  }

  def write_pdf(
    file: JFile, paint: Graphics2D => Unit, width: Int, height: Int)
  {
    import com.lowagie.text.{Document, Rectangle}

    val out = new BufferedOutputStream(new FileOutputStream(file))
    try {
      val document = new Document()
      try {
        document.setPageSize(new Rectangle(width, height))
        val writer = PdfWriter.getInstance(document, out)
        document.open()

        val cb = writer.getDirectContent()
        val tp = cb.createTemplate(width, height)
        val gfx = tp.createGraphics(width, height, font_mapper())

        paint(gfx)
        gfx.dispose

        cb.addTemplate(tp, 1, 0, 0, 1, 0, 0)
      }
      finally { document.close() }
    }
    finally { out.close }
  }

  def write_pdf(path: Path, paint: Graphics2D => Unit, width: Int, height: Int): Unit =
    write_pdf(path.file, paint, width, height)

  def write_pdf(file: JFile, chart: JFreeChart, width: Int, height: Int)
  {
    def paint(gfx: Graphics2D) = chart.draw(gfx, new Rectangle2D.Double(0, 0, width, height))
    write_pdf(file, paint _, width, height)
  }

  def write_pdf(path: Path, chart: JFreeChart, width: Int, height: Int): Unit =
    write_pdf(path.file, chart, width, height)
}
