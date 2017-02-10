/*  Title:      Pure/System/isabelle_charset.scala
    Author:     Makarius

Charset for Isabelle symbols.
*/

package isabelle


import java.nio.Buffer
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder, CoderResult}
import java.nio.charset.spi.CharsetProvider


object Isabelle_Charset
{
  val name: String = "UTF-8-Isabelle-test"  // FIXME
  lazy val charset: Charset = new Isabelle_Charset
}


class Isabelle_Charset extends Charset(Isabelle_Charset.name, null)
{
  override def contains(cs: Charset): Boolean =
    cs.name.equalsIgnoreCase(UTF8.charset_name) || UTF8.charset.contains(cs)

  override def newDecoder(): CharsetDecoder = UTF8.charset.newDecoder

  override def newEncoder(): CharsetEncoder = UTF8.charset.newEncoder
}


class Isabelle_Charset_Provider extends CharsetProvider
{
  override def charsetForName(name: String): Charset =
  {
    // FIXME inactive
    // if (name.equalsIgnoreCase(Isabelle_Charset.name)) Isabelle_Charset.charset
    // else null
    null
  }

  override def charsets(): java.util.Iterator[Charset] =
  {
    import scala.collection.JavaConversions._
    // FIXME inactive
    // Iterator(Isabelle_Charset.charset)
    Iterator()
  }
}

