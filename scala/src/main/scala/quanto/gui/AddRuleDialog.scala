package quanto.gui

import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, ValueChanged}
import quanto.data._
import quanto.util.Globals

import scala.util.matching
import scala.util.matching.Regex

import quanto.util.UserOptions.{scale, scaleInt}

class AddRuleDialog(project: Project) extends Dialog {
  modal = true

  implicit def buttonIsSelected(radButton: RadioButton): Boolean = radButton.selected
  var cancelled = false
  def result: Seq[RuleDesc] = {
    if(!cancelled) {
      val unsorted = if (MainPanel.radIncludeForwards) {
        MainPanel.selection.map(s => RuleDesc(s))
      } else if (MainPanel.radIncludeInverse) {
        MainPanel.selection.map(s => RuleDesc(s, inverse = true))
      } else {
        MainPanel.selection.flatMap(s => Seq(RuleDesc(s), RuleDesc(s, inverse = true)))
      }
      unsorted.sortBy(rd => rd.name)
    } else {
      Seq()
    }
  }

  val AddButton = new Button("Add")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AddButton)

  //  val dir = Files.newDirectoryStream(Paths.get(rootDir), "**/*.qrule")
  //  for (p <- dir.asScala) println(p)


  val MainPanel = new BoxPanel(Orientation.Vertical) {
    val FList = new FilteredList(project.filesEndingIn(".qrule"))
    contents += FList
    def selection : List[String] = FList.ListComponent.selection.items.toList
    val radIncludeForwards = new RadioButton("Forwards")
    radIncludeForwards.selected = true
    val radIncludeInverse = new RadioButton("Inverted")
    var radIncludeInverseAndForwards= new RadioButton("Both")
    val radGroupIncludeInverse = new ButtonGroup(radIncludeForwards, radIncludeInverse, radIncludeInverseAndForwards)

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (AddButton, Swing.HStrut(5), CancelButton)
    }

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), radIncludeForwards, Swing.HStrut(10))
      contents += (Swing.HStrut(10), radIncludeInverse, Swing.HStrut(10))
      contents += (Swing.HStrut(10), radIncludeInverseAndForwards, Swing.HStrut(10))
    }

    contents += Swing.VStrut(10)

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
