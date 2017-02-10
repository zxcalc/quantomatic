/*  Title:      Pure/General/file.scala
    Author:     Makarius

File system operations.
*/

package isabelle


import java.io.{BufferedWriter, OutputStreamWriter, FileOutputStream, BufferedOutputStream,
  OutputStream, InputStream, FileInputStream, BufferedInputStream, BufferedReader,
  InputStreamReader, File => JFile, IOException}
import java.nio.file.{StandardOpenOption, StandardCopyOption, Path => JPath,
  Files, SimpleFileVisitor, FileVisitResult}
import java.nio.file.attribute.BasicFileAttributes
import java.net.{URL, URLDecoder, MalformedURLException}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import java.util.regex.Pattern

import org.tukaani.xz.{XZInputStream, XZOutputStream}

import scala.collection.mutable
import scala.util.matching.Regex


object File
{
  /* standard path (Cygwin or Posix) */

  def standard_path(path: Path): String = path.expand.implode

  def standard_path(platform_path: String): String =
    if (Platform.is_windows) {
      val Platform_Root = new Regex("(?i)" +
        Pattern.quote(Isabelle_System.cygwin_root()) + """(?:\\+|\z)(.*)""")
      val Drive = new Regex("""([a-zA-Z]):\\*(.*)""")

      platform_path.replace('/', '\\') match {
        case Platform_Root(rest) => "/" + rest.replace('\\', '/')
        case Drive(letter, rest) =>
          "/cygdrive/" + Word.lowercase(letter) +
            (if (rest == "") "" else "/" + rest.replace('\\', '/'))
        case path => path.replace('\\', '/')
      }
    }
    else platform_path

  def standard_path(file: JFile): String = standard_path(file.getPath)

  def path(file: JFile): Path = Path.explode(standard_path(file))

  def standard_url(name: String): String =
    try {
      val url = new URL(name)
      if (url.getProtocol == "file")
        standard_path(URLDecoder.decode(url.getPath, UTF8.charset_name))
      else name
    }
    catch { case _: MalformedURLException => standard_path(name) }


  /* platform path (Windows or Posix) */

  private val Cygdrive = new Regex("/cygdrive/([a-zA-Z])($|/.*)")
  private val Named_Root = new Regex("//+([^/]*)(.*)")

  def platform_path(standard_path: String): String =
    if (Platform.is_windows) {
      val result_path = new StringBuilder
      val rest =
        standard_path match {
          case Cygdrive(drive, rest) =>
            result_path ++= (Word.uppercase(drive) + ":" + JFile.separator)
            rest
          case Named_Root(root, rest) =>
            result_path ++= JFile.separator
            result_path ++= JFile.separator
            result_path ++= root
            rest
          case path if path.startsWith("/") =>
            result_path ++= Isabelle_System.cygwin_root()
            path
          case path => path
        }
      for (p <- space_explode('/', rest) if p != "") {
        val len = result_path.length
        if (len > 0 && result_path(len - 1) != JFile.separatorChar)
          result_path += JFile.separatorChar
        result_path ++= p
      }
      result_path.toString
    }
    else standard_path

  def platform_path(path: Path): String = platform_path(standard_path(path))
  def platform_file(path: Path): JFile = new JFile(platform_path(path))

  def platform_url(raw_path: Path): String =
  {
    val path = raw_path.expand
    require(path.is_absolute)
    val s = platform_path(path).replaceAll(" ", "%20")
    if (!Platform.is_windows) "file://" + s
    else if (s.startsWith("\\\\")) "file:" + s.replace('\\', '/')
    else "file:///" + s.replace('\\', '/')
  }


  /* bash path */

  def bash_path(path: Path): String = Bash.string(standard_path(path))
  def bash_path(file: JFile): String = Bash.string(standard_path(file))


  /* directory entries */

  def check_dir(path: Path): Path =
    if (path.is_dir) path else error("No such directory: " + path)

  def check_file(path: Path): Path =
    if (path.is_file) path else error("No such file: " + path)


  /* directory content */

  def read_dir(dir: Path): List[String] =
  {
    if (!dir.is_dir) error("Bad directory: " + dir.toString)
    val files = dir.file.listFiles
    if (files == null) Nil
    else files.toList.map(_.getName)
  }

  def find_files(start: JFile, pred: JFile => Boolean = _ => true): List[JFile] =
  {
    val result = new mutable.ListBuffer[JFile]

    if (start.isFile && pred(start)) result += start
    else if (start.isDirectory) {
      Files.walkFileTree(start.toPath,
        new SimpleFileVisitor[JPath] {
          override def visitFile(path: JPath, attrs: BasicFileAttributes): FileVisitResult =
          {
            val file = path.toFile
            if (pred(file)) result += file
            FileVisitResult.CONTINUE
          }
        }
      )
    }

    result.toList
  }


  /* read */

  def read(file: JFile): String = Bytes.read(file).toString
  def read(path: Path): String = read(path.file)


  def read_stream(reader: BufferedReader): String =
  {
    val output = new StringBuilder(100)
    var c = -1
    while ({ c = reader.read; c != -1 }) output += c.toChar
    reader.close
    output.toString
  }

  def read_stream(stream: InputStream): String =
    read_stream(new BufferedReader(new InputStreamReader(stream, UTF8.charset)))

  def read_gzip(file: JFile): String =
    read_stream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(file))))
  def read_gzip(path: Path): String = read_gzip(path.file)

  def read_xz(file: JFile): String =
    read_stream(new XZInputStream(new BufferedInputStream(new FileInputStream(file))))
  def read_xz(path: Path): String = read_xz(path.file)


  /* read lines */

  def read_lines(reader: BufferedReader, progress: String => Unit): List[String] =
  {
    val result = new mutable.ListBuffer[String]
    var line: String = null
    while ({ line = try { reader.readLine} catch { case _: IOException => null }; line != null }) {
      progress(line)
      result += line
    }
    reader.close
    result.toList
  }


  /* write */

  def write_file(file: JFile, text: CharSequence, make_stream: OutputStream => OutputStream)
  {
    val stream = make_stream(new FileOutputStream(file))
    val writer = new BufferedWriter(new OutputStreamWriter(stream, UTF8.charset))
    try { writer.append(text) } finally { writer.close }
  }

  def write(file: JFile, text: CharSequence): Unit = write_file(file, text, s => s)
  def write(path: Path, text: CharSequence): Unit = write(path.file, text)

  def write_gzip(file: JFile, text: CharSequence): Unit =
    write_file(file, text, (s: OutputStream) => new GZIPOutputStream(new BufferedOutputStream(s)))
  def write_gzip(path: Path, text: CharSequence): Unit = write_gzip(path.file, text)

  def write_xz(file: JFile, text: CharSequence, options: XZ.Options): Unit =
    File.write_file(file, text, s => new XZOutputStream(new BufferedOutputStream(s), options))
  def write_xz(file: JFile, text: CharSequence): Unit = write_xz(file, text, XZ.options())
  def write_xz(path: Path, text: CharSequence, options: XZ.Options): Unit =
    write_xz(path.file, text, options)
  def write_xz(path: Path, text: CharSequence): Unit = write_xz(path, text, XZ.options())

  def write_backup(path: Path, text: CharSequence)
  {
    if (path.is_file) move(path, path.backup)
    write(path, text)
  }

  def write_backup2(path: Path, text: CharSequence)
  {
    if (path.is_file) move(path, path.backup2)
    write(path, text)
  }


  /* append */

  def append(file: JFile, text: CharSequence): Unit =
    Files.write(file.toPath, UTF8.bytes(text.toString),
      StandardOpenOption.APPEND, StandardOpenOption.CREATE)

  def append(path: Path, text: CharSequence): Unit = append(path.file, text)


  /* eq */

  def eq(file1: JFile, file2: JFile): Boolean =
    try { java.nio.file.Files.isSameFile(file1.toPath, file2.toPath) }
    catch { case ERROR(_) => false }

  def eq(path1: Path, path2: Path): Boolean = eq(path1.file, path2.file)


  /* copy */

  def copy(src: JFile, dst: JFile)
  {
    val target = if (dst.isDirectory) new JFile(dst, src.getName) else dst
    if (!eq(src, target))
      Files.copy(src.toPath, target.toPath,
        StandardCopyOption.COPY_ATTRIBUTES,
        StandardCopyOption.REPLACE_EXISTING)
  }

  def copy(path1: Path, path2: Path): Unit = copy(path1.file, path2.file)


  /* move */

  def move(src: JFile, dst: JFile)
  {
    val target = if (dst.isDirectory) new JFile(dst, src.getName) else dst
    if (!eq(src, target))
      Files.move(src.toPath, target.toPath, StandardCopyOption.REPLACE_EXISTING)
  }

  def move(path1: Path, path2: Path): Unit = move(path1.file, path2.file)
}
