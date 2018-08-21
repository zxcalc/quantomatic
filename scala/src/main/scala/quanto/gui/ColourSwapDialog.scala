package quanto.gui

import quanto.data.Theory.ValueType

import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, ValueChanged}
import quanto.data._
import quanto.util.{Globals, UserOptions}

import scala.util.matching
import scala.util.matching.Regex
import quanto.util.UserOptions.{scale, scaleInt}

class ColourSwapDialog(theory: Theory) extends Dialog {
  modal = true
  title = "Specify new colours"
  val smallGap: Int = UserOptions.scaleInt(5)

  val types : List[String] = theory.vertexTypes.keys.toList

  val SwapButton = new Button("Swap")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(SwapButton)

  def result: Map[String, String] = {
    if(!cancelled) {
      types.map(t => t -> comboFields(t).selection.item).toMap
    } else {
      types.map(t => t -> t).toMap
    }
  }

  var cancelled = false

  val comboFields : Map[String, ComboBox[String]] = types.map(t => {
    t -> new ComboBox(types)
  }).toMap

  val MainPanel = new BoxPanel(Orientation.Vertical) {

    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(smallGap),
        new Label("Specify which types are sent to which"),
        Swing.HStrut(smallGap))
    }
    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(smallGap),
        new GridPanel(types.length, 2) {
          types.foreach(t => {
            val label = new Label(t)
            label.horizontalAlignment = Alignment.Left
            contents += label
            val tf = comboFields(t)
            contents += tf
            tf.selection.item = t
          })
        },
        Swing.HStrut(smallGap))
    }
    contents += Swing.VStrut(smallGap)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (SwapButton, Swing.HStrut(smallGap), CancelButton)
    }

    contents += Swing.VStrut(2*smallGap)

  }


  contents = MainPanel

  listenTo(SwapButton, CancelButton)

  reactions += {
    case ButtonClicked(SwapButton) =>
      cancelled = false
      close()
    case ButtonClicked(CancelButton) =>
      cancelled = true
      close()
  }
}
