/*  Title:      Pure/PIDE/text.scala
    Module:     PIDE
    Author:     Fabian Immler, TU Munich
    Author:     Makarius

Basic operations on plain text.
*/

package isabelle


import scala.collection.mutable
import scala.util.Sorting


object Text
{
  /* offset */

  type Offset = Int


  /* range -- with total quasi-ordering */

  object Range
  {
    def apply(start: Offset): Range = Range(start, start)

    val offside: Range = apply(-1)

    object Ordering extends scala.math.Ordering[Text.Range]
    {
      def compare(r1: Text.Range, r2: Text.Range): Int = r1 compare r2
    }
  }

  sealed case class Range(val start: Offset, val stop: Offset)
  {
    // denotation: {start} Un {i. start < i & i < stop}
    if (start > stop)
      error("Bad range: [" + start.toString + ":" + stop.toString + "]")

    override def toString = "[" + start.toString + ":" + stop.toString + "]"

    def length: Int = stop - start

    def map(f: Offset => Offset): Range = Range(f(start), f(stop))
    def +(i: Offset): Range = if (i == 0) this else map(_ + i)
    def -(i: Offset): Range = if (i == 0) this else map(_ - i)

    def is_singularity: Boolean = start == stop
    def inflate_singularity: Range = if (is_singularity) Range(start, start + 1) else this

    def contains(i: Offset): Boolean = start == i || start < i && i < stop
    def contains(that: Range): Boolean = this.contains(that.start) && that.stop <= this.stop
    def overlaps(that: Range): Boolean = this.contains(that.start) || that.contains(this.start)
    def compare(that: Range): Int = if (overlaps(that)) 0 else this.start compare that.start

    def apart(that: Range): Boolean =
      (this.start max that.start) > (this.stop min that.stop)

    def restrict(that: Range): Range =
      Range(this.start max that.start, this.stop min that.stop)

    def try_restrict(that: Range): Option[Range] =
      if (this apart that) None
      else Some(restrict(that))

    def try_join(that: Range): Option[Range] =
      if (this apart that) None
      else Some(Range(this.start min that.start, this.stop max that.stop))
  }


  /* perspective */

  object Perspective
  {
    val empty: Perspective = Perspective(Nil)

    def full: Perspective = Perspective(List(Range(0, Integer.MAX_VALUE / 2)))

    def apply(ranges: Seq[Range]): Perspective =
    {
      val result = new mutable.ListBuffer[Text.Range]
      var last: Option[Text.Range] = None
      def ship(next: Option[Range]) { result ++= last; last = next }

      for (range <- ranges.sortBy(_.start))
      {
        last match {
          case None => ship(Some(range))
          case Some(last_range) =>
            last_range.try_join(range) match {
              case None => ship(Some(range))
              case joined => last = joined
            }
        }
      }
      ship(None)
      new Perspective(result.toList)
    }
  }

  final class Perspective private(
    val ranges: List[Range]) // visible text partitioning in canonical order
  {
    def is_empty: Boolean = ranges.isEmpty
    def range: Range =
      if (is_empty) Range(0)
      else Range(ranges.head.start, ranges.last.stop)

    override def hashCode: Int = ranges.hashCode
    override def equals(that: Any): Boolean =
      that match {
        case other: Perspective => ranges == other.ranges
        case _ => false
      }
    override def toString = ranges.toString
  }


  /* information associated with text range */

  sealed case class Info[A](val range: Text.Range, val info: A)
  {
    def restrict(r: Text.Range): Info[A] = Info(range.restrict(r), info)
    def try_restrict(r: Text.Range): Option[Info[A]] = range.try_restrict(r).map(Info(_, info))
  }

  type Markup = Info[XML.Elem]


  /* editing */

  object Edit
  {
    def insert(start: Offset, text: String): Edit = new Edit(true, start, text)
    def remove(start: Offset, text: String): Edit = new Edit(false, start, text)
  }

  final class Edit private(val is_insert: Boolean, val start: Offset, val text: String)
  {
    override def toString =
      (if (is_insert) "Insert(" else "Remove(") + (start, text).toString + ")"


    /* transform offsets */

    private def transform(do_insert: Boolean, i: Offset): Offset =
      if (i < start) i
      else if (do_insert) i + text.length
      else (i - text.length) max start

    def convert(i: Offset): Offset = transform(is_insert, i)
    def revert(i: Offset): Offset = transform(!is_insert, i)


    /* edit strings */

    private def insert(i: Offset, string: String): String =
      string.substring(0, i) + text + string.substring(i)

    private def remove(i: Offset, count: Int, string: String): String =
      string.substring(0, i) + string.substring(i + count)

    def can_edit(string: String, shift: Int): Boolean =
      shift <= start && start < shift + string.length

    def edit(string: String, shift: Int): (Option[Edit], String) =
      if (!can_edit(string, shift)) (Some(this), string)
      else if (is_insert) (None, insert(start - shift, string))
      else {
        val i = start - shift
        val count = text.length min (string.length - i)
        val rest =
          if (count == text.length) None
          else Some(Edit.remove(start, text.substring(count)))
        (rest, remove(i, count, string))
      }
  }
}
