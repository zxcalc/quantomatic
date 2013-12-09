package quanto.gui

import quanto.gui._
import quanto.layout._
import quanto.layout.constraint._
import quanto.data._
import Names._
import graphview.GraphView
import swing._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.UIManager
import quanto.util.json.Json
import java.io.File
import javax.swing.JOptionPane


object LayoutTestGUI1 extends SimpleSwingApplication {

  var json : Json = _
  
  
  val graphView = new GraphView(Theory.DefaultTheory) {
    drawGrid = true
    focusable = true
  }
  
  val layout = new ForceLayout with Clusters with Ranking with VerticalBoundary
  layout.alpha0 = 0.2

  layout.initialize(graphView.graph)

  def choosePlainFile(title: String = ""): Option[File] = {  
    val chooser = new FileChooser(new File("."))
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      println("Approve -- " + chooser.selectedFile)
      Some(chooser.selectedFile)
    } else None
  } 
  val timer = new javax.swing.Timer(50, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      layout.step()
      layout.updateGraph()
      graphView.graph = layout.graph
      graphView.invalidateGraph()
      graphView.repaint()

    }
  })

  def top = new MainFrame {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
     menuBar = new MenuBar {
         contents += new Menu("File"){
           contents += new MenuItem(Action("Open"){
             val choose = choosePlainFile("input")
             choose match  {
               case Some(file) => {println("file loaded")
            	   				/// clear the panel
                 
            	   				   graphView.init
            	   				   json = Json.parse(file)
                 				   graphView.graph = Graph.fromJson(json)
                           layout.initialize(graphView.graph)

                 				// start to draw 
                 				   timer.start()
                 				   }
               case None => JOptionPane.showMessageDialog(self, "Invalid File", "Error", 0)
             }                        
           })
         }
       }
    
    title = "GraphView"
    contents = graphView
    size = new Dimension(800,600)
    peer.setLocationRelativeTo(null)
  }
}
