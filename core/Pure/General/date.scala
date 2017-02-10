/*  Title:      Pure/General/date.scala
    Author:     Makarius

Date and time, with time zone.
*/

package isabelle


import java.util.Locale
import java.time.{Instant, ZonedDateTime, LocalTime, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.temporal.TemporalAccessor

import scala.annotation.tailrec


object Date
{
  /* format */

  object Format
  {
    def make(fmts: List[DateTimeFormatter], tune: String => String = s => s): Format =
    {
      require(fmts.nonEmpty)

      new Format {
        def apply(date: Date): String = fmts.head.format(date.rep)
        def parse(str: String): Date =
          new Date(ZonedDateTime.from(Formatter.try_variants(fmts, tune(str))))
      }
    }

    def apply(pats: String*): Format =
      make(pats.toList.map(Date.Formatter.pattern(_)))

    val default: Format = Format("dd-MMM-uuuu HH:mm:ss xx")
    val date: Format = Format("dd-MMM-uuuu")
    val time: Format = Format("HH:mm:ss")
  }

  abstract class Format private
  {
    def apply(date: Date): String
    def parse(str: String): Date

    def unapply(str: String): Option[Date] =
      try { Some(parse(str)) } catch { case _: DateTimeParseException => None }
  }

  object Formatter
  {
    def pattern(pat: String): DateTimeFormatter = DateTimeFormatter.ofPattern(pat)

    def variants(pats: List[String], locs: List[Locale] = Nil): List[DateTimeFormatter] =
      pats.flatMap(pat => {
        val fmt = pattern(pat)
        if (locs.isEmpty) List(fmt) else locs.map(fmt.withLocale(_))
      })

    @tailrec def try_variants(fmts: List[DateTimeFormatter], str: String,
      last_exn: Option[DateTimeParseException] = None): TemporalAccessor =
    {
      fmts match {
        case Nil =>
          throw last_exn.getOrElse(new DateTimeParseException("Failed to parse date", str, 0))
        case fmt :: rest =>
          try { ZonedDateTime.from(fmt.parse(str)) }
          catch { case exn: DateTimeParseException => try_variants(rest, str, Some(exn)) }
      }
    }
  }


  /* date operations */

  def timezone(): ZoneId = ZoneId.systemDefault

  def now(zone: ZoneId = timezone()): Date = new Date(ZonedDateTime.now(zone))

  def apply(t: Time, zone: ZoneId = timezone()): Date =
    new Date(ZonedDateTime.ofInstant(t.instant, zone))
}

sealed case class Date(rep: ZonedDateTime)
{
  def midnight: Date =
    new Date(ZonedDateTime.of(rep.toLocalDate, LocalTime.MIDNIGHT, rep.getZone))

  def to(zone: ZoneId): Date = new Date(rep.withZoneSameInstant(zone))
  def to_utc: Date = to(ZoneId.of("UTC"))

  def time: Time = Time.instant(Instant.from(rep))
  def timezone: ZoneId = rep.getZone

  def format(fmt: Date.Format = Date.Format.default): String = fmt(this)
  override def toString: String = format()
}
