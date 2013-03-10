package quanto.gui.test

import swing._
import quanto.gui.histview.HistView
import quanto.util.test.SimpleTreeSeq


object HistViewTest extends SimpleSwingApplication {
//  var tree = new SimpleTreeSeq[String]()
//  tree = tree :+ ("first", None)
//  tree = tree :+ ("second", Some("first"))
//  tree = tree :+ ("third", Some("second"))
//  tree = tree :+ ("fourth", Some("second"))
//  tree = tree :+ ("fifth", Some("third"))
//  tree = tree :+ ("sixth", Some("second"))
//  tree = tree :+ ("seventh", Some("second"))
//  tree = tree :+ ("seventh.5", Some("seventh"))
//  tree = tree :+ ("seventh.6", Some("seventh.5"))
//  tree = tree :+ ("eighth", Some("fourth"))
//  tree = tree :+ ("ninth", Some("fourth"))

  //val str = "some really long string name "

  var tree = new SimpleTreeSeq[Int]()
  val rand = new java.util.Random()
  tree :+= (0, None)
  for (i <- 1 to 200) {
    val parent = math.max(0, i - 1 - rand.nextInt(6))
    tree :+= (i, Some(parent))
  }

  println("tree built")

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
