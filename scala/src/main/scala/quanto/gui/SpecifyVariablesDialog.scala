package quanto.gui

import quanto.data.Theory.ValueType

import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, ValueChanged}
import quanto.data._
import quanto.util.{Globals, UserOptions}

import scala.util.matching
import scala.util.matching.Regex
import quanto.util.UserOptions.{scale, scaleInt}

class SpecifyVariablesDialog(variables: List[(ValueType, String)]) extends Dialog {
  modal = true
  title = "Specify values for introduced variables?"
  val smallGap: Int = UserOptions.scaleInt(5)



  val AddButton = new Button("Add")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AddButton)

  def result: Map[(ValueType, String), String] = {
    if(!cancelled) {
      variables.map(sv => sv -> textFields(sv).text).toMap
    } else {
      variables.map(sv => sv -> sv._2).toMap
    }
  }



  implicit def buttonIsSelected(radButton: RadioButton): Boolean = radButton.selected
  var cancelled = false


  //  val dir = Files.newDirectoryStream(Paths.get(rootDir), "**/*.qrule")
  //  for (p <- dir.asScala) println(p)
  val textFields : Map[(ValueType, String), TextField] = variables.map(name => {
    name -> new TextField
  }).toMap

  val MainPanel = new BoxPanel(Orientation.Vertical) {

    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(smallGap),
        new Label("This rule introduces new variables. Would you like to specify values?"),
        Swing.HStrut(smallGap))
    }
    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(smallGap),
      new GridPanel(variables.length, 2) {
        variables.foreach(name => {
          val label = new Label(s"${name._2} (${TheoryEditPanel.valueTypesAsHumanReadable(name._1)})")
          label.horizontalAlignment = Alignment.Left
          contents += label
          val tf = textFields(name)
          contents += tf
          tf.text = name._2
        })
      },
        Swing.HStrut(smallGap))
    }
    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (AddButton, Swing.HStrut(smallGap), CancelButton)
    }

    contents += Swing.VStrut(2*smallGap)

  }


  contents = MainPanel

  listenTo(AddButton, CancelButton)

  reactions += {
    case ButtonClicked(AddButton) =>
      cancelled = false
      close()
    case ButtonClicked(CancelButton) =>
      cancelled = true
      close()
  }
}
