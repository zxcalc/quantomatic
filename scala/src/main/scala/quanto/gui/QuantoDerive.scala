package quanto.gui

import scala.swing._
import java.awt.Dimension
import scala.swing.event.{ButtonClicked, Key}
import javax.swing.{Box, BoxLayout, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.Json
import quanto.data.Theory


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  object Split extends SplitPane {
    orientation = Orientation.Vertical
    contents_=(new BorderPanel(), new BorderPanel())
  }

  class NewDialog extends Dialog {
    val newDialog = this
    modal = true
    val NameField = new TextField()
    val LocationField = new TextField()
    val BrowseButton = new Button("...")
    // TODO: make these not hard-coded
    val TheoryField = new ComboBox(List("red_green", "strategy_graph", "string_ve"))
    val CreateButton = new Button("Create")
    val CancelButton = new Button("Cancel")
    defaultButton = Some(CreateButton)

    var result : Option[(Theory,String,String)] = None

    val mainPanel = new BoxPanel(Orientation.Vertical) {

      contents += Swing.VStrut(10)

      contents += new BoxPanel(Orientation.Horizontal) {
        val nameLabel = new Label("Name", null, Alignment.Right)
        nameLabel.preferredSize = new Dimension(80, 30)
        LocationField.preferredSize = new Dimension(235, 30)

        contents += Swing.HStrut(10)
        contents += nameLabel
        contents += Swing.HStrut(5)
        contents += NameField
        contents += Swing.HStrut(10)
      }

      contents += Swing.VStrut(10)

      contents += new BoxPanel(Orientation.Horizontal) {
        val locationLabel = new Label("Location", null, Alignment.Right)
        locationLabel.preferredSize = new Dimension(80, 30)
        LocationField.preferredSize = new Dimension(200, 30)
        BrowseButton.preferredSize = new Dimension(30, 30)

        contents += Swing.HStrut(10)
        contents += locationLabel
        contents += Swing.HStrut(5)
        contents += LocationField
        contents += Swing.HStrut(5)
        contents += BrowseButton
        contents += Swing.HStrut(10)
      }

      contents += Swing.VStrut(10)

      contents += new BoxPanel(Orientation.Horizontal) {
        val theoryLabel = new Label("Theory ", null, Alignment.Right)
        theoryLabel.preferredSize = new Dimension(80, 30)
        TheoryField.preferredSize = new Dimension(235, 30)

        contents += Swing.HStrut(10)
        contents += theoryLabel
        contents += Swing.HStrut(5)
        contents += TheoryField
        contents += Swing.HStrut(10)
      }

      contents += Swing.VStrut(10)

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += CreateButton
        contents += Swing.HStrut(5)
        contents += CancelButton
      }

      contents += Swing.VStrut(10)
    }

    contents = mainPanel

    listenTo(BrowseButton, CreateButton, CancelButton)

    reactions += {
      case ButtonClicked(CreateButton) =>
        val thyFile = new Json.Input(Theory.getClass.getResourceAsStream(TheoryField.selection + ".qtheory"))
        result = Some((Theory.fromJson(Json.parse(thyFile)), NameField.text, LocationField.text))
        close()
      case ButtonClicked(CancelButton) =>
        close()
      case ButtonClicked(BrowseButton) =>
        val chooser = new FileChooser()
        chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        chooser.showOpenDialog(mainPanel) match {
          case FileChooser.Result.Approve =>
            LocationField.text = chooser.selectedFile.toString
          case _ =>
        }
    }
  }

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      def apply() {
        val d = new NewDialog()
        d.centerOnScreen()
        d.open()
        d.result.map {
          case (thy,name,path) =>
            printf("got a theory and location")
        }

        val x = 2
      }
    }

    val OpenProjectAction = new Action("Open Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, CommandMask | Key.Modifier.Shift))
      def apply() {

      }
    }
  }

  def top = new MainFrame {
    title = "QuantoDerive"
    contents = Split

    size = new Dimension(1000,800)

    menuBar = new MenuBar {
      contents += (FileMenu)
    }
  }
}
