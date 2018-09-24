package quanto.data

import scala.swing.Publisher
import scala.swing.event.Event

abstract class GraphEvent extends Event

case class GraphChanged(sender: HasGraph) extends GraphEvent

// will cause any graph views to invalidate and repaint the graph
case class GraphReplaced(sender: HasGraph, clearSelection: Boolean) extends GraphEvent

trait HasGraph extends Publisher {
  def graph: Graph = gr

  def graph_=(g: Graph) {
    gr = g
    publish(GraphChanged(this))
  }

  protected def gr: Graph

  protected def gr_=(g: Graph)
}
