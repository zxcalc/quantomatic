package quanto.gui

import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, ValueChanged}
import quanto.data._
import quanto.util.Globals

import scala.util.matching
import scala.util.matching.Regex


class AddRuleDialog(project: Project) extends Dialog {
  modal = true

  implicit def buttonIsSelected(radButton: RadioButton) : Boolean = radButton.selected

  def result: Seq[RuleDesc] = {
    val unsorted = if (MainPanel.radIncludeForwards) {
      MainPanel.FilteredRuleList.selection.items.map(s => RuleDesc(s))
    } else if (MainPanel.radIncludeInverse) {
      MainPanel.FilteredRuleList.selection.items.map(s => RuleDesc(s, inverse = true))
    } else {
      MainPanel.FilteredRuleList.selection.items.flatMap(s => Seq(RuleDesc(s), RuleDesc(s, inverse = true)))
    }
    unsorted.sortBy(rd => rd.name)
  }

  val AddButton = new Button("Add")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AddButton)

//  val dir = Files.newDirectoryStream(Paths.get(rootDir), "**/*.qrule")
//  for (p <- dir.asScala) println(p)

  val MainPanel = new BoxPanel(Orientation.Vertical) {
    val Search = new TextField
    val InitialRules : Vector[String] = project.rules.sorted

    var FilteredRuleList : ListView[String] = new ListView[String](InitialRules)

    val RulePane = new ScrollPane(FilteredRuleList)
    val radIncludeForwards = new RadioButton("Forwards")
    radIncludeForwards.selected = true
    val radIncludeInverse = new RadioButton("Inverted")
    var radIncludeInverseAndForwards= new RadioButton("Both")
    val radGroupIncludeInverse = new ButtonGroup(radIncludeForwards, radIncludeInverse, radIncludeInverseAndForwards)
    RulePane.preferredSize = new Dimension(400,200)

    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), new Label("Filter:"), Swing.HStrut(5), Search, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), RulePane, Swing.HStrut(10))
    }
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), radIncludeForwards, Swing.HStrut(10))
      contents += (Swing.HStrut(10), radIncludeInverse, Swing.HStrut(10))
      contents += (Swing.HStrut(10), radIncludeInverseAndForwards, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (AddButton, Swing.HStrut(5), CancelButton)
    }

    contents += Swing.VStrut(10)
  }

  contents = MainPanel

  listenTo(AddButton, CancelButton, MainPanel.Search)

  reactions += {
    case ButtonClicked(AddButton) =>
      close()
    case ButtonClicked(CancelButton) =>
      MainPanel.FilteredRuleList.selection.indices.clear()
      close()
    case ValueChanged(MainPanel.Search) =>
      try {
        MainPanel.FilteredRuleList.listData = MainPanel.InitialRules.filter(
          s => s.matches(".*" + MainPanel.Search.text + ".*"))
      } catch {
        case e: Exception =>
          //Exceptions here are thrown by inelligable regex from the user
      }
  }
}
