package quanto.gui.test

import swing._
import quanto.gui.histview.{HistNode, HistView}
import quanto.util.test.SimpleTreeSeq
import java.awt.Color


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

  case class IntNode(i: Int) extends HistNode {
    def label = i.toString
    def color = if (i % 4 == 0) Color.RED else Color.WHITE
  }

  case class StringNode(s: String) extends HistNode {
    def label = s.toString
    def color = if (s == "*") Color.WHITE else Color.BLACK
  }

//  var tree = new SimpleTreeSeq[IntNode]()
//  val rand = new java.util.Random()
//  tree :+= (IntNode(0), None)
//  for (i <- 1 to 200) {
//    val parent = math.max(0, i - 1 - rand.nextInt(6))
//    tree :+= (IntNode(i), Some(IntNode(parent)))
//  }

  var tree = new SimpleTreeSeq[StringNode]()
  tree :+= (StringNode("rule1"), None)
  tree :+= (StringNode("rule2"), Some(StringNode("rule1")))
  tree :+= (StringNode("rule3"), Some(StringNode("rule2")))
  tree :+= (StringNode("*"), Some(StringNode("rule3")))
  tree :+= (StringNode("rule4"), Some(StringNode("rule2")))
tree :+= (StringNode("*"), Some(StringNode("rule4")))

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
