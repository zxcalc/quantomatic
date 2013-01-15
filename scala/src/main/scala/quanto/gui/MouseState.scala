package quanto.gui

import java.awt.Point
import java.awt.geom.Rectangle2D
import math.{min,abs}
import quanto.data._

sealed abstract class MouseState

class InvalidMouseStateException(val when: String, val state: MouseState)
extends Exception("Got unexpected mouse state: " + state + ", when: " + when)

case class SelectTool() extends MouseState
case class SelectionBox(start: Point, end: Point) extends MouseState {
  def rect = {
    new Rectangle2D.Double(
      min(start.getX, end.getX),
      min(start.getY, end.getY),
      abs(end.getX - start.getX),
      abs(end.getY - start.getY)
    )
  }
}
case class DragVertex(start: Point, end: Point) extends MouseState

case class AddVertexTool() extends MouseState
case class AddEdgeTool(directed: Boolean) extends MouseState
case class DragEdge(directed: Boolean, startVertex: VName) extends MouseState
