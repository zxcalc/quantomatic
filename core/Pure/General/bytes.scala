/*  Title:      Pure/General/bytes.scala
    Author:     Makarius

Immutable byte vectors versus UTF8 strings.
*/

package isabelle


import java.io.{File => JFile, ByteArrayOutputStream, ByteArrayInputStream,
  OutputStream, InputStream, FileInputStream, FileOutputStream}

import org.tukaani.xz.{XZInputStream, XZOutputStream}


object Bytes
{
  val empty: Bytes = new Bytes(Array[Byte](), 0, 0)

  def apply(s: CharSequence): Bytes =
  {
    val str = s.toString
    if (str.isEmpty) empty
    else {
      val b = UTF8.bytes(str)
      new Bytes(b, 0, b.length)
    }
  }

  def apply(a: Array[Byte]): Bytes = apply(a, 0, a.length)

  def apply(a: Array[Byte], offset: Int, length: Int): Bytes =
    if (length == 0) empty
    else {
      val b = new Array[Byte](length)
      System.arraycopy(a, offset, b, 0, length)
      new Bytes(b, 0, b.length)
    }


  /* read */

  def read_stream(stream: InputStream, limit: Int = Integer.MAX_VALUE, hint: Int = 1024): Bytes =
    if (limit == 0) empty
    else {
      val out = new ByteArrayOutputStream(if (limit == Integer.MAX_VALUE) hint else limit)
      val buf = new Array[Byte](8192)
      var m = 0

      do {
        m = stream.read(buf, 0, buf.size min (limit - out.size))
        if (m != -1) out.write(buf, 0, m)
      } while (m != -1 && limit > out.size)

      new Bytes(out.toByteArray, 0, out.size)
    }

  def read(file: JFile): Bytes =
    using(new FileInputStream(file))(read_stream(_, file.length.toInt))

  def read(path: Path): Bytes = read(path.file)


  /* write */

  def write(file: JFile, bytes: Bytes)
  {
    val stream = new FileOutputStream(file)
    try { bytes.write_stream(stream) } finally { stream.close }
  }

  def write(path: Path, bytes: Bytes): Unit = write(path.file, bytes)
}

final class Bytes private(
  protected val bytes: Array[Byte],
  protected val offset: Int,
  val length: Int) extends CharSequence
{
  /* equality */

  override def equals(that: Any): Boolean =
  {
    that match {
      case other: Bytes =>
        if (this eq other) true
        else if (length != other.length) false
        else (0 until length).forall(i => bytes(offset + i) == other.bytes(other.offset + i))
      case _ => false
    }
  }

  private lazy val hash: Int =
  {
    var h = 0
    for (i <- offset until offset + length) {
      val b = bytes(i).asInstanceOf[Int] & 0xFF
      h = 31 * h + b
    }
    h
  }

  override def hashCode(): Int = hash


  /* content */

  lazy val sha1_digest: SHA1.Digest = SHA1.digest(bytes)

  override def toString: String =
  {
    val str = UTF8.decode_chars(s => s, bytes, offset, offset + length).toString
    if (str.contains('\uFFFD')) "Bytes(" + length + ")" else str
  }

  def isEmpty: Boolean = length == 0

  def +(other: Bytes): Bytes =
    if (other.isEmpty) this
    else if (isEmpty) other
    else {
      val new_bytes = new Array[Byte](length + other.length)
      System.arraycopy(bytes, offset, new_bytes, 0, length)
      System.arraycopy(other.bytes, other.offset, new_bytes, length, other.length)
      new Bytes(new_bytes, 0, new_bytes.length)
    }


  /* CharSequence operations */

  def charAt(i: Int): Char =
    if (0 <= i && i < length) (bytes(offset + i).asInstanceOf[Int] & 0xFF).asInstanceOf[Char]
    else throw new IndexOutOfBoundsException

  def subSequence(i: Int, j: Int): Bytes =
  {
    if (0 <= i && i <= j && j <= length) new Bytes(bytes, offset + i, j - i)
    else throw new IndexOutOfBoundsException
  }


  /* streams */

  def stream(): ByteArrayInputStream = new ByteArrayInputStream(bytes, offset, length)

  def write_stream(stream: OutputStream): Unit = stream.write(bytes, offset, length)


  /* XZ data compression */

  def uncompress(): Bytes =
    using(new XZInputStream(stream()))(Bytes.read_stream(_, hint = length))

  def compress(options: XZ.Options = XZ.options()): Bytes =
  {
    val result = new ByteArrayOutputStream(length)
    using(new XZOutputStream(result, options))(write_stream(_))
    new Bytes(result.toByteArray, 0, result.size)
  }
}
