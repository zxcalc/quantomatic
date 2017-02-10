/*  Title:      Pure/General/time.scala
    Author:     Makarius

Time based on milliseconds.
*/

package isabelle


import java.util.Locale
import java.time.Instant


object Time
{
  def seconds(s: Double): Time = new Time((s * 1000.0).round)
  def minutes(s: Double): Time = new Time((s * 60000.0).round)
  def ms(m: Long): Time = new Time(m)
  def hms(h: Int, m: Int, s: Double): Time = seconds(s + 60 * m + 3600 * h)

  def now(): Time = ms(System.currentTimeMillis())

  val zero: Time = ms(0)
  val start: Time = now()

  def print_seconds(s: Double): String =
    String.format(Locale.ROOT, "%.3f", s.asInstanceOf[AnyRef])

  def instant(t: Instant): Time = ms(t.getEpochSecond * 1000L + t.getNano / 1000000L)
}

final class Time private(val ms: Long) extends AnyVal
{
  def seconds: Double = ms / 1000.0
  def minutes: Double = ms / 60000.0

  def + (t: Time): Time = new Time(ms + t.ms)
  def - (t: Time): Time = new Time(ms - t.ms)

  def compare(t: Time): Int = ms compare t.ms
  def < (t: Time): Boolean = ms < t.ms
  def <= (t: Time): Boolean = ms <= t.ms
  def > (t: Time): Boolean = ms > t.ms
  def >= (t: Time): Boolean = ms >= t.ms

  def min(t: Time): Time = if (this < t) this else t
  def max(t: Time): Time = if (this > t) this else t

  def is_zero: Boolean = ms == 0
  def is_relevant: Boolean = ms >= 1

  override def toString: String = Time.print_seconds(seconds)

  def message: String = toString + "s"

  def message_hms: String =
  {
    val s = ms / 1000
    String.format(Locale.ROOT, "%d:%02d:%02d",
      new java.lang.Long(s / 3600), new java.lang.Long((s / 60) % 60), new java.lang.Long(s % 60))
  }

  def instant: Instant = Instant.ofEpochMilli(ms)
}
