/*  Title:      Pure/General/xz_file.scala
    Author:     Makarius

XZ file system operations.
*/

package isabelle


import java.io.{BufferedOutputStream, OutputStream, FileInputStream, BufferedInputStream,
  File => JFile}

import org.tukaani.xz.{LZMA2Options, XZInputStream, XZOutputStream}


object XZ_File
{
  def read(file: JFile): String =
    File.read_stream(new XZInputStream(new BufferedInputStream(new FileInputStream(file))))

  def read(path: Path): String = read(path.file)

  def write(file: JFile, text: Iterable[CharSequence], preset: Int = 3)
  {
    val options = new LZMA2Options
    options.setPreset(preset)
    File.write_file(file, text, (s: OutputStream) =>
      new XZOutputStream(new BufferedOutputStream(s), options))
  }
}

