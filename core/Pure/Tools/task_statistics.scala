/*  Title:      Pure/Tools/task_statistics.scala
    Author:     Makarius

Future task runtime statistics.
*/

package isabelle


import scala.swing.{Frame, Component}

import org.jfree.data.statistics.HistogramDataset
import org.jfree.chart.{JFreeChart, ChartPanel, ChartFactory}
import org.jfree.chart.plot.{XYPlot, PlotOrientation}
import org.jfree.chart.renderer.xy.{XYBarRenderer, StandardXYBarPainter}


object Task_Statistics
{
  def apply(name: String, tasks: List[Properties.T]): Task_Statistics =
    new Task_Statistics(name, tasks)

  def apply(info: Build.Log_Info): Task_Statistics =
    apply(info.name, info.tasks)
}

final class Task_Statistics private(val name: String, val tasks: List[Properties.T])
{
  private val Task_Name = new Properties.String("task_name")
  private val Run = new Properties.Int("run")

  def chart(bins: Int = 100): JFreeChart =
  {
    val values = new Array[Double](tasks.length)
    for ((Run(x), i) <- tasks.iterator.zipWithIndex)
      values(i) = java.lang.Math.log10((x max 1).toDouble / 1000000)

    val data = new HistogramDataset
    data.addSeries("tasks", values, bins)

    val c =
      ChartFactory.createHistogram("Task runtime distribution",
        "log10(runtime / s)", "number of tasks", data,
        PlotOrientation.VERTICAL, true, true, true)

    val renderer = c.getPlot.asInstanceOf[XYPlot].getRenderer.asInstanceOf[XYBarRenderer]
    renderer.setMargin(0.1)
    renderer.setBarPainter(new StandardXYBarPainter)

    c
  }

  def show_frame(bins: Int = 100): Unit =
    GUI_Thread.later {
      new Frame {
        iconImage = GUI.isabelle_image()
        title = name
        contents = Component.wrap(new ChartPanel(chart(bins)))
        visible = true
      }
    }
}

