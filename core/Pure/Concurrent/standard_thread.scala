/*  Title:      Pure/Concurrent/standard_thread.scala
    Author:     Makarius

Standard thread operations.
*/

package isabelle


import java.lang.Thread
import java.util.concurrent.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue, ThreadFactory}


object Standard_Thread
{
  /* fork */

  def fork(name: String = "", daemon: Boolean = false)(body: => Unit): Thread =
  {
    val thread =
      if (name == null || name == "") new Thread() { override def run = body }
      else new Thread(name) { override def run = body }
    thread.setDaemon(daemon)
    thread.start
    thread
  }


  /* pool */

  lazy val pool: ThreadPoolExecutor =
    {
      val m = Value.Int.unapply(System.getProperty("isabelle.threads", "0")) getOrElse 0
      val n = if (m > 0) m else (Runtime.getRuntime.availableProcessors max 1) min 8
      val executor =
        new ThreadPoolExecutor(n, n, 2500L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable])
      val old_thread_factory = executor.getThreadFactory
      executor.setThreadFactory(
        new ThreadFactory {
          def newThread(r: Runnable) =
          {
            val thread = old_thread_factory.newThread(r)
            thread.setDaemon(true)
            thread
          }
        })
      executor
    }


  /* delayed events */

  final class Delay private[Standard_Thread](first: Boolean, delay: => Time, event: => Unit)
  {
    private var running: Option[Event_Timer.Request] = None

    private def run: Unit =
    {
      val do_run = synchronized {
        if (running.isDefined) { running = None; true } else false
      }
      if (do_run) event
    }

    def invoke(): Unit = synchronized
    {
      val new_run =
        running match {
          case Some(request) => if (first) false else { request.cancel; true }
          case None => true
        }
      if (new_run)
        running = Some(Event_Timer.request(Time.now() + delay)(run))
    }

    def revoke(): Unit = synchronized
    {
      running match {
        case Some(request) => request.cancel; running = None
        case None =>
      }
    }

    def postpone(alt_delay: Time): Unit = synchronized
    {
      running match {
        case Some(request) =>
          val alt_time = Time.now() + alt_delay
          if (request.time < alt_time && request.cancel) {
            running = Some(Event_Timer.request(alt_time)(run))
          }
        case None =>
      }
    }
  }

  // delayed event after first invocation
  def delay_first(delay: => Time)(event: => Unit): Delay = new Delay(true, delay, event)

  // delayed event after last invocation
  def delay_last(delay: => Time)(event: => Unit): Delay = new Delay(false, delay, event)
}
