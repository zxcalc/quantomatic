package quanto.gui

import swing._
import swing.event._
import scala.language.postfixOps

class DetailDispaly {
  private val panel = new BoxPanel(Orientation.Vertical){
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }
  private val detailFrame = new Frame{
    title = "Details";

    val nameL = new Label ("Name: ");
    val nameT = new TextField ("To Complete")
    val name = new FlowPanel(FlowPanel.Alignment.Left)()
    name.contents += (nameL, nameT)

    val nodeTypeL = new Label ("Type: ");
    val nodeTypeT = new TextField ("To Complete")
    val nodeType = new FlowPanel(FlowPanel.Alignment.Left)()
    nodeType.contents += (nodeTypeL, nodeTypeT)

    val updateButton = new Button ("Update")
    panel.contents += (name, nodeType, updateButton)
    contents = panel
  }
  detailFrame.size_=(new Dimension(300,200))
  val _ = detailFrame.visible_=(true)

  def showDetails /*(strl: List[String])*/() = {
    //var label1 = new Label ("i'm new 1");
    //var label2 = new Label ("i'm new 2");
    //panel.contents.clear();
    //List(label1, label2) map (panel.contents +=);
  }

  def visible_= (b : Boolean){
    detailFrame.visible_= (b)
  }

}