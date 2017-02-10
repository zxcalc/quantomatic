/*  Title:      Pure/Concurrent/event_timer.scala
    Author:     Makarius

Initiate event after given point in time.

Note: events are run as synchronized action within a dedicated thread
and should finish quickly without further ado.
*/

package isabelle


import java.util.{Timer, TimerTask, Date => JDate}


object Event_Timer
{
  private lazy val event_timer = new Timer("event_timer", true)

  final class Request private[Event_Timer](val time: Time, task: TimerTask)
  {
    def cancel: Boolean = task.cancel
  }

  def request(time: Time)(event: => Unit): Request =
  {
    val task = new TimerTask { def run { event } }
    event_timer.schedule(task, new JDate(time.ms))
    new Request(time, task)
  }
}

