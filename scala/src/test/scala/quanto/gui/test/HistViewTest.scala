package quanto.gui.test

import swing._
import quanto.gui.histview.HistView
import quanto.util.test.SimpleTreeSeq


object HistViewTest extends SimpleSwingApplication {
  var tree = new SimpleTreeSeq[String]()
  tree = tree :+ ("first", None)
  tree = tree :+ ("second", Some("first"))
  tree = tree :+ ("third", Some("second"))
  tree = tree :+ ("fourth", Some("second"))
  tree = tree :+ ("fifth", Some("third"))

  val main = new BorderPanel {
    add(new ScrollPane(new HistView(tree)), BorderPanel.Position.Center)
    preferredSize = new Dimension(200,450)
  }

  def top = new MainFrame {
    title = "HistView"
    size = new Dimension(200,450)
    contents = main
  }
}
