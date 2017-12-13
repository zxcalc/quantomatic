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
  ScrollContainer.preferredSize = new Dimension(scaleInt(baseWidth), scaleInt(baseHeight))

  private def VSpace: Component = Swing.VStrut(scaleInt(10))

  private def HSpace: Component = Swing.HStrut(scaleInt(10))

  contents += VSpace
  contents += new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, new Label("Filter:"), HSpace, Regex, HSpace)
  }
  contents += VSpace
  contents += new BoxPanel(Orientation.Horizontal) {
    contents += (HSpace, ScrollContainer, HSpace)
  }

  listenTo(Regex)
  reactions += {
    case ValueChanged(Regex) =>
      try {
        ListComponent.listData = listItems.filter(
          s => s.matches(".*" + Regex.text + ".*"))
      } catch {
        case e: Exception =>
        //Exceptions here are thrown by inelligable regex from the user
      }
  }
}
