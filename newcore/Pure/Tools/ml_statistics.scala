/*  Title:      Pure/Tools/ml_statistics.scala
    Author:     Makarius

ML runtime statistics.
*/

package isabelle


import scala.collection.mutable
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.swing.{Frame, Component}

import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{JFreeChart, ChartPanel, ChartFactory}
import org.jfree.chart.plot.PlotOrientation


object ML_Statistics
{
  /* content interpretation */

  final case class Entry(time: Double, data: Map[String, Double])

  def apply(name: String, stats: List[Properties.T]): ML_Statistics =
    new ML_Statistics(name, stats)

  def apply(info: Build.Log_Info): ML_Statistics =
    apply(info.name, info.stats)

  val empty = apply("", Nil)


  /* standard fields */

  val tasks_fields =
    ("Future tasks", List("tasks_ready", "tasks_pending", "tasks_running", "tasks_passive"))

  val workers_fields =
    ("Worker threads", List("workers_total", "workers_active", "workers_waiting"))

  val GC_fields = ("GCs", List("partial_GCs", "full_GCs"))

  val heap_fields =
    ("Heap", List("size_heap", "size_allocation", "size_allocation_free",
      "size_heap_free_last_full_GC", "size_heap_free_last_GC"))

  val threads_fields =
    ("Threads", List("threads_total", "threads_in_ML", "threads_wait_condvar",
      "threads_wait_IO", "threads_wait_mutex", "threads_wait_signal"))

  val time_fields = ("Time", List("time_CPU", "time_GC"))

  val speed_fields = ("Speed", List("speed_CPU", "speed_GC"))

  val standard_fields =
    List(tasks_fields, workers_fields, GC_fields, heap_fields, threads_fields,
      time_fields, speed_fields)
}

final class ML_Statistics private(val name: String, val stats: List[Properties.T])
{
  val Now = new Properties.Double("now")
  def now(props: Properties.T): Double = Now.unapply(props).get

  require(stats.forall(props => Now.unapply(props).isDefined))

  val time_start = if (stats.isEmpty) 0.0 else now(stats.head)
  val duration = if (stats.isEmpty) 0.0 else now(stats.last) - time_start

  val fields: Set[String] =
    SortedSet.empty[String] ++
      (for (props <- stats.iterator; (x, _) <- props.iterator if x != Now.name)
        yield x)

  val content: List[ML_Statistics.Entry] =
  {
    var last_edge = Map.empty[String, (Double, Double, Double)]
    val result = new mutable.ListBuffer[ML_Statistics.Entry]
    for (props <- stats) {
      val time = now(props) - time_start
      require(time >= 0.0)

      // rising edges -- relative speed
      val speeds =
        for ((key, value) <- props; a <- Library.try_unprefix("time", key)) yield {
          val (x0, y0, s0) = last_edge.getOrElse(a, (0.0, 0.0, 0.0))

          val x1 = time
          val y1 = java.lang.Double.parseDouble(value)
          val s1 = if (x1 == x0) 0.0 else (y1 - y0) / (x1 - x0)

          val b = ("speed" + a).intern
          if (y1 > y0) { last_edge += (a -> (x1, y1, s1)); (b, s1) } else (b, s0)
        }

      val data =
        SortedMap.empty[String, Double] ++ speeds ++
          (for ((x, y) <- props.iterator if x != Now.name)
            yield (x, java.lang.Double.parseDouble(y)))
      result += ML_Statistics.Entry(time, data)
    }
    result.toList
  }


  /* charts */

  def update_data(data: XYSeriesCollection, selected_fields: Iterable[String])
  {
    data.removeAllSeries
    for {
      field <- selected_fields.iterator
      series = new XYSeries(field)
    } {
      content.foreach(entry => series.add(entry.time, entry.data(field)))
      data.addSeries(series)
    }
  }

  def chart(title: String, selected_fields: Iterable[String]): JFreeChart =
  {
    val data = new XYSeriesCollection
    update_data(data, selected_fields)

    ChartFactory.createXYLineChart(title, "time", "value", data,
      PlotOrientation.VERTICAL, true, true, true)
  }

  def chart(arg: (String, Iterable[String])): JFreeChart = chart(arg._1, arg._2)

  def show_standard_frames(): Unit =
    ML_Statistics.standard_fields.map(chart(_)).foreach(c =>
      GUI_Thread.later {
        new Frame {
          iconImage = GUI.isabelle_image()
          title = name
          contents = Component.wrap(new ChartPanel(c))
          visible = true
        }
      })
}

