import controlP5.*;
import javax.swing.*;

ControlP5 gui;
PFont helvetica;
PFont times;

Vertex selectedVertex=null;
int selectedIndex=-1;
char tool;
JFileChooser fileChooser;
Graph graph;

QuantoBack backend;
XMLReader xml;

void setup() {
  size(800,600, JAVA2D);
  smooth();
  frameRate(30);
  gui = new ControlP5(this);
  gui.addTextlabel("sel", "SELECT", 10, 10)
    .setColorValue(0xffff0000);
  gui.addTextlabel("mv", "MOVE", 10, 10)
    .setColorValue(0xffff0000);
  gui.addTextlabel("ed", "EDGE", 10, 10)
    .setColorValue(0xffff0000);
    
  try { 
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()); 
  } catch (Exception e) { 
    e.printStackTrace();  
  }
  fileChooser = new JFileChooser();
  
  helvetica = loadFont("HelveticaNeue-14.vlw");
  times = loadFont("Times-Italic-14.vlw");

  graph = new Graph();
  tool = 's';

  // just some testing code here
  Vertex H = new Vertex("testH", 100,100);
  H.setColor("H");
  graph.addVertex(H);

  Vertex bnd = new Vertex("boundary", 100,200);
  bnd.setColor("boundary");
  graph.addVertex(bnd);

  Vertex red = new Vertex("testR", 200,100);
  red.setColor("red");
  red.setAngle("\u03B1 + \u03B2");
  graph.addVertex(red);

  Vertex green = new Vertex("testG", 200,200);
  green.setColor("green");
  green.setAngle("x");
  graph.addVertex(green);

  backend = new QuantoBack();
  xml = new XMLReader();
}

void mousePressed() {
  Vertex n;
  Iterator it;
  switch (tool) {
    case 's':
      selectedVertex = null;
      for (int i=0; i<graph.vertexList.size();++i) {
        n = (Vertex)graph.vertexList.get(i);
        n.selected = false;
        if (n.at(mouseX, mouseY)) {
          selectedVertex = n;
          selectedIndex = i;
        }
      }
      if (selectedVertex!=null) selectedVertex.selected = true;
      break;
    case 'm':
      if (selectedVertex != null) selectedVertex.setDest(mouseX, mouseY);
      break;
      case 'e':
      it = graph.vertices.values().iterator();
      while (it.hasNext()) {
        n = (Vertex)it.next();
        if (n.at(mouseX, mouseY)) {
	    modifyGraph("e "+selectedVertex.id+" "+n.id+"\n");
	}
      }
      break;
  }
}

void keyPressed() {
  if (tool == key) tool = 's';
  else if (key == 'l') layout(graph);
  else if (key == 'o') {
    if (fileChooser.showOpenDialog(this)==JFileChooser.APPROVE_OPTION) {
      File f = fileChooser.getSelectedFile();
      String[] contents = loadStrings(f);
      StringBuffer accum = new StringBuffer();
      for (int j=0;j<contents.length;++j) accum.append(contents[j]);
      layout(accum.toString(), graph);
    }
  } else if (key == 'r' // red 
	     || key == 'g' // green
	     || key == 'h' // hadamard
	     || key == 'b' // boundary
	     || key == 'n' // new graph
	     || key == 'u' // undo 
	     ) {
      /* all the commands the back end knows how to do we just pass on */
      modifyGraph(key + "");

  } else if (key == 'd') { // delete node
      if (selectedVertex!=null) modifyGraph("d "+selectedVertex.id);      
  } else if (key == 'q') {
      println("Shutting down quantoML");
      backend.send("Q\n");
      println(backend.receive());
      backend.send("quit () ; \n");
      println("Quitting....");
      exit();
  } else if (key==TAB) {
    if (graph.vertexList.size()>0) {
      selectedIndex = (selectedIndex+1)%graph.vertexList.size();
      if (selectedVertex!=null) selectedVertex.selected = false;
      selectedVertex = (Vertex)graph.vertexList.get(selectedIndex);
      selectedVertex.selected = true;
    }
  } else tool = key;
}

void modifyGraph(String cmd) {
      backend.send(cmd + "\n");
      println(backend.receive());
      // here send D to backend and dump the graph
      // then rebuild it via the XML parser.
      backend.send("D\n");
      Graph updated = xml.parseGraph(backend.receive());
      updated.reconcileVertexCoords(graph);
      this.graph = updated;
      graph.layoutGraph();
}    

void draw() {
  background(255);
  
  gui.controller("mv").hide();
  gui.controller("sel").hide();
  gui.controller("ed").hide();
  
  switch (tool) {
    case 's':
     gui.controller("sel").show();
     break;
    case 'm':
     gui.controller("mv").show();
     break;
    case 'e':
     gui.controller("ed").show();
     break;
  }
  
  Edge e;
  for (int i=0; i<graph.edgeList.size(); ++i) {
    e = (Edge)graph.edgeList.get(i);
    e.display();
  }
  
  Vertex n;
  Iterator i = graph.vertices.values().iterator();
  while (i.hasNext()) {
    n = (Vertex)i.next();
    n.tick();
    n.display();
  }
  
}
