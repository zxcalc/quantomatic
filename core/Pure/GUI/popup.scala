/*  Title:      Pure/GUI/popup.scala
    Module:     PIDE-GUI
    Author:     Makarius

Popup within layered pane.
*/

package isabelle


import java.awt.{Point, Dimension}
import javax.swing.{JLayeredPane, JComponent}


class Popup(
  layered: JLayeredPane,
  component: JComponent,
  location: Point,
  size: Dimension)
{
  def show
  {
    component.setLocation(location)
    component.setSize(size)
    component.setPreferredSize(size)
    component.setOpaque(true)
    layered.add(component, JLayeredPane.DEFAULT_LAYER)
    layered.moveToFront(component)
    layered.repaint(component.getBounds())
  }

  def hide
  {
    val bounds = component.getBounds()
    layered.remove(component)
    layered.repaint(bounds)
  }
}

