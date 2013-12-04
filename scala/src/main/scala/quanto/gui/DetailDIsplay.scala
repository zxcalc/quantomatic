package quanto.gui

import swing._
import swing.event._
import scala.language.postfixOps
import quanto.data._

class DetailDisplay extends Frame {
  private val panel = new BoxPanel(Orientation.Vertical){
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

    title = "Details";


    val nameL = new Label ("Name: ");
    val nameT = new TextField ("                    ")
    val name =  new BoxPanel(Orientation.Horizontal)
    name.contents += (nameL, nameT)

    val nodeTypeL = new Label ("Type: ");
    val nodeTypeT = new TextField ("                     ")
    val nodeType = new BoxPanel(Orientation.Horizontal)
    nodeType.contents += (nodeTypeL, nodeTypeT)

    val updateButton = new Button ("Update")

    val prefixHGraph = "Hierarchy: "
    val hgraphTraceL = new Label (prefixHGraph + "main")
    panel.contents += (name, nodeType, hgraphTraceL/*, updateButton*/)
    contents = panel

    var node : VName = null

    /*
    * TODO:  allow to edit
    * tmporarily disable the editing just for now
    * */
    nameT.editable_= (false);
    nodeTypeT.editable_= (false);


  size_=(new Dimension(300,120))
  visible_=(true)

  def showDetails (v : VName, name: String, typ : String)() = {
    this.node = v

    //TODO: resize the text filed to make it visiable
    this.nameT.text_= (name)
    this.nodeTypeT.text_= (typ)
    //var label1 = new Label ("i'm new 1");
    //var label2 = new Label ("i'm new 2");
    //panel.contents.clear();
    //List(label1, label2) map (panel.contents +=);
  }

  def showHierachy (h : String) = {
     this.hgraphTraceL.text_= (prefixHGraph + h)
  }

}