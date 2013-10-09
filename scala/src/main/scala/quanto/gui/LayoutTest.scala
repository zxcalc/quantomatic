package quanto.gui

import quanto.gui._
import quanto.layout._
import quanto.layout.constraint._
import quanto.data._
import Names._

import graphview.GraphView
import swing._
import java.awt.event.{ActionEvent, ActionListener}
import scala.util._

object LayoutTest extends SimpleSwingApplication {

  val graphView = new GraphView(Theory.DefaultTheory) {
    drawGrid = true
    dynamicResize = false
    focusable = true
  }
  
  graphView.graph = (Graph()
	 addVertex("v00", NodeV())
	 addVertex("v01", NodeV())
	 addVertex("v02", NodeV())
	 addEdge("e01" , DirEdge(), "v00" -> "v01")
	 addEdge("e02" , DirEdge(), "v00" -> "v02")
	 
	 addVertex("v03", NodeV())
	 addVertex("v04", NodeV())
	 addEdge("e03" , DirEdge(), "v01" -> "v03")
	 addEdge("e04" , DirEdge(), "v01" -> "v04")
	 
	 addVertex("v05", NodeV())
	 addVertex("v06", NodeV())
	 
	 addEdge("e05" , DirEdge(), "v02" -> "v05")
	 addEdge("e06" , DirEdge(), "v02" -> "v06")
  )
  	
  	graphView.graph= (Graph()
		addVertex("v00", NodeV() withCoord(0,0))
		addVertex("v10", NodeV() withCoord(0,0))
		addVertex("v20", NodeV() withCoord(0,0))
		addVertex("v30", NodeV() withCoord(0,0))
		addVertex("v01", NodeV() withCoord(0,0))
		addVertex("v11", NodeV() withCoord(0,0))
		addVertex("v21", NodeV() withCoord(0,0))
		addVertex("v31", NodeV() withCoord(0,0))
		addVertex("v02", NodeV() withCoord(0,0))
		addVertex("v12", NodeV() withCoord(0,0))
		addVertex("v22", NodeV() withCoord(0,0))
		addVertex("v32", NodeV() withCoord(0,0))
		addVertex("v03", NodeV() withCoord(0,0))
		addVertex("v13", NodeV() withCoord(0,0))
		addVertex("v23", NodeV() withCoord(0,0))
		addVertex("v33", NodeV() withCoord(0,0))
		
		addEdge("e00h", DirEdge(), ("v00") -> ("v10"))
		addEdge("e10h", DirEdge(), ("v10") -> ("v20"))
		addEdge("e20h", DirEdge(), ("v20") -> ("v30"))
		    
		addEdge("e01h", DirEdge(), ("v01") -> ("v11"))
		addEdge("e11h", DirEdge(), ("v11") -> ("v21"))
		addEdge("e21h", DirEdge(), ("v21") -> ("v31"))
		
		addEdge("e02h", DirEdge(), ("v02") -> ("v12"))
		addEdge("e12h", DirEdge(), ("v12") -> ("v22"))
		addEdge("e22h", DirEdge(), ("v22") -> ("v32"))
		
		addEdge("e03h", DirEdge(), ("v03") -> ("v13"))
		addEdge("e13h", DirEdge(), ("v13") -> ("v23"))
		addEdge("e23h", DirEdge(), ("v23") -> ("v33"))
		
		addEdge("e00v", DirEdge(), ("v00") -> ("v01"))
		addEdge("e01v", DirEdge(), ("v10") -> ("v11"))
	  	addEdge("e02v", DirEdge(), ("v20") -> ("v21"))
	  	addEdge("e03v", DirEdge(), ("v30") -> ("v31"))
		
		addEdge("e10v", DirEdge(), ("v01") -> ("v02"))
		addEdge("e11v", DirEdge(), ("v11") -> ("v12"))
		addEdge("e12v", DirEdge(), ("v21") -> ("v22"))
		addEdge("e13v", DirEdge(), ("v31") -> ("v32"))
		
		addEdge("e20v", DirEdge(), ("v02") -> ("v03"))
		addEdge("e21v", DirEdge(), ("v12") -> ("v13"))
		addEdge("e22v", DirEdge(), ("v22") -> ("v23"))
		addEdge("e23v", DirEdge(), ("v32") -> ("v33"))

		
	  	addVertex("start", WireV() withCoord (0,0))
	  	addVertex("end", WireV() withCoord (0,0))
	  	addEdge("s", DirEdge(), "start" -> "v00")
	  	addEdge("en", DirEdge(), "v33" -> "end")


  	)
  	
  	
  	
//  	
//  graphView.graph = (Graph()
//    addVertex ("b0", WireV() withCoord(-0.13234,-0.126))
//    addVertex ("b1", WireV() withCoord(-0.245,-0.23452))
//    addVertex ("b2", WireV() withCoord(-0.3345,-0.33654))
//    addVertex ("b3", WireV() withCoord(-0.42,-0.434523))
//    addVertex ("v0", NodeV() withCoord (0.03452,0.2456))
//    addVertex ("v1", NodeV() withCoord (0.1253,0.1354))
//    addVertex ("v2", NodeV() withCoord (0.235232,0.234253))
//    addVertex ("v3", NodeV() withCoord (0.323453,0.3767))
//    addEdge   ("e0", DirEdge(), "v0" -> "v2")
//    addEdge   ("e1", DirEdge(), "v0" -> "v3")
//    addEdge   ("e2", DirEdge(), "v1" -> "v2")
//    addEdge   ("e3", DirEdge(), "v1" -> "v3")
//    addEdge   ("e4", DirEdge(), "b0" -> "v0")
//    addEdge   ("e5", DirEdge(), "b1" -> "v1")
//    addEdge   ("e6", DirEdge(), "v2" -> "b2")
//    addEdge   ("e7", DirEdge(), "v3" -> "b3")
////    addBBox   ("bb1", BBData(), Set("b0","v3"))
////    addBBox   ("bb2", BBData(), Set("b0","v1"))
////    addBBox   ("bb3", BBData(), Set("v0","v3"))
//  )
  
  val rnd = new Random()
  
  
  graphView.graph = graphView.graph.verts.foldLeft(graphView.graph) {
 (g,v) => g.updateVData(v) { _.withCoord((rnd.nextDouble,rnd.nextDouble)) }
}

//  println(graphView.graph.toString)
//  println(graphView.graph.succVerts("v0"))
//  
//  graphView.graph = Graph.random(20,20,1)

  val layout = new ForceLayout  with VerticalBoundary with Ranking with Clusters     
  layout.alpha0 = 0.2

  layout.initialize(graphView.graph)
  var run = 0
  var constraints = false

  val timer = new javax.swing.Timer(50, new ActionListener {
    def actionPerformed(e: ActionEvent) {

      layout.step()

      constraints = !constraints

      layout.updateGraph()
      graphView.graph = layout.graph
      graphView.invalidateGraph()
      graphView.repaint()

//      if (layout.prevEnergy < layout.energy) {
//        print("+")
//      } else if (layout.prevEnergy > layout.energy) {
//        print("-")
//      } else {
//        print("=")
//      }
//      run += 1
//      if (run % 80 == 0) println()
    }
  })

  def top = new MainFrame {
    title = "GraphView"
    contents = graphView
    size = new Dimension(800,800)

    timer.start()
  }
}
