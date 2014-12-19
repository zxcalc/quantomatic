/*  Title:      Pure/General/sha1.scala
    Module:     PIDE
    Author:     Makarius

Digest strings according to SHA-1 (see RFC 3174).
*/

package isabelle


import java.io.{File => JFile, FileInputStream}
import java.security.MessageDigest


object SHA1
{
  final class Digest private[SHA1](val rep: String)
  {
    override def hashCode: Int = rep.hashCode
    override def equals(that: Any): Boolean =
      that match {
        case other: Digest => rep == other.rep
        case _ => false
      }
    override def toString: String = rep
  }

  private def make_result(digest: MessageDigest): Digest =
  {
    val result = new StringBuilder
    for (b <- digest.digest()) {
      val i = b.asInstanceOf[Int] & 0xFF
      if (i < 16) result += '0'
      result ++= Integer.toHexString(i)
    }
    new Digest(result.toString)
  }

  def digest(file: JFile): Digest =
  {
    val stream = new FileInputStream(file)
    val digest = MessageDigest.getInstance("SHA")

    val buf = new Array[Byte](65536)
    var m = 0
    try {
      do {
        m = stream.read(buf, 0, buf.length)
        if (m != -1) digest.update(buf, 0, m)
      } while (m != -1)
    }
    finally { stream.close }

    make_result(digest)
  }

  def digest(bytes: Array[Byte]): Digest =
  {
    val digest = MessageDigest.getInstance("SHA")
    digest.update(bytes)

    make_result(digest)
  }

  def digest(bytes: Bytes): Digest = bytes.sha1_digest

  def digest(string: String): Digest = digest(Bytes(string))

  def fake(rep: String): Digest = new Digest(rep)
}

