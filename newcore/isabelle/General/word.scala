/*  Title:      Pure/General/word.scala
    Module:     PIDE
    Author:     Makarius

Support for words within Unicode text.
*/

package isabelle


import java.util.Locale


object Word
{
  /* codepoints */

  def codepoint_iterator(str: String): Iterator[Int] =
    new Iterator[Int] {
      var offset = 0
      def hasNext: Boolean = offset < str.length
      def next: Int =
      {
        val c = str.codePointAt(offset)
        offset += Character.charCount(c)
        c
      }
    }

  def codepoint(c: Int): String = new String(Array(c), 0, 1)


  /* case */

  def lowercase(str: String): String = str.toLowerCase(Locale.ROOT)
  def uppercase(str: String): String = str.toUpperCase(Locale.ROOT)

  def capitalize(str: String): String =
    if (str.length == 0) str
    else {
      val n = Character.charCount(str.codePointAt(0))
      uppercase(str.substring(0, n)) + lowercase(str.substring(n))
    }

  def perhaps_capitalize(str: String): String =
    if (codepoint_iterator(str).forall(c => Character.isLowerCase(c) || Character.isDigit(c)))
      capitalize(str)
    else str

  sealed abstract class Case
  case object Lowercase extends Case
  case object Uppercase extends Case
  case object Capitalized extends Case

  object Case
  {
    def apply(c: Case, str: String): String =
      c match {
        case Lowercase => lowercase(str)
        case Uppercase => uppercase(str)
        case Capitalized => capitalize(str)
      }
    def unapply(str: String): Option[Case] =
      if (!str.isEmpty) {
        if (codepoint_iterator(str).forall(Character.isLowerCase(_))) Some(Lowercase)
        else if (codepoint_iterator(str).forall(Character.isUpperCase(_))) Some(Uppercase)
        else {
          val it = codepoint_iterator(str)
          if (Character.isUpperCase(it.next) && it.forall(Character.isLowerCase(_)))
            Some(Capitalized)
          else None
        }
      }
      else None
  }


  /* sequence of words */

  def implode(words: Iterable[String]): String = words.iterator.mkString(" ")

  def explode(sep: Char => Boolean, text: String): List[String] =
    Library.separated_chunks(sep, text).map(_.toString).filter(_ != "").toList

  def explode(sep: Char, text: String): List[String] =
    explode(_ == sep, text)

  def explode(text: String): List[String] =
    explode(Character.isWhitespace(_), text)
}

