package quanto.gui

import quanto.util.UserOptions.scaleInt

import scala.swing.event.ValueChanged
import scala.swing.{BoxPanel, Component, Dimension, Label, ListView, Orientation, ScrollPane, Swing, TextField}

// Create a list of strings to select from, with a regex filter box at the top
// Regex: A text box that contains the regex to filter by
// listItems: The list to filter on
// ListComponent: The UI Component that the user selects elements from
class FilteredList(val options: List[String],
                   baseWidth: Int = 400,
                   baseHeight: Int = 200) extends BoxPanel(Orientation.Vertical) {
  val Regex = new TextField
  val listItems: List[String] = options.sorted
  val ListComponent: ListView[String] = new ListView[String](listItems)
  private val ScrollContainer = new ScrollPane(ListComponent)
  ScrollContainer.maximumSize = new Dimension(scaleInt(baseWidth), scaleInt(baseHeight))

  private def VSpace: Component = Swing.VStrut(scaleInt(10))

  private def HSpace: Component = Swing.HStrut(scaleInt(10))

  contents += VSpace
  val FilterPanel : Component = new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, new Label("Filter:"), HSpace, Regex, HSpace)
    maximumSize = new Dimension(scaleInt(baseWidth), scaleInt(20))
  }

  contents += FilterPanel
  contents += VSpace
  contents += new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, ScrollContainer, HSpace)
  }

  listenTo(Regex)
  reactions += {
    case ValueChanged(Regex) =>
      try {
        val filtered = listItems.filter(
          s => s.matches("(?i).*" + Regex.text + ".*"))
        ListComponent.listData = filtered
        if(filtered.nonEmpty) {
          ListComponent.peer.setSelectionInterval(0, filtered.length - 1)
        }
        if(Regex.text.isEmpty){
          ListComponent.peer.clearSelection()
        }
      } catch {
        case e: Exception =>
        //Exceptions here are thrown by inelligable regex from the user
      }
  }
}
