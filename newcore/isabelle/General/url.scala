/*  Title:      Pure/General/url.scala
    Author:     Makarius

Basic URL operations.
*/

package isabelle


import java.net.{URL, MalformedURLException}


object Url
{
  def apply(name: String): URL =
  {
    try { new URL(name) }
    catch { case _: MalformedURLException => error("Malformed URL " + quote(name)) }
  }

  def is_wellformed(name: String): Boolean =
    try { Url(name); true }
    catch { case ERROR(_) => false }

  def is_readable(name: String): Boolean =
    try { Url(name).openStream.close; true }
    catch { case ERROR(_) => false }

  def read(name: String): String =
  {
    val stream = Url(name).openStream
    try { File.read_stream(stream) }
    finally { stream.close }
  }
}

