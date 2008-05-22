import controlP5.*;
import javax.swing.*;

ControlP5 gui;
PFont helvetica;
PFont times;

Node selectedNode=null;
int selectedIndex=-1;
char tool;
JFileChooser fileChooser;
Graph graph;

QuantoBack backend;

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
  Node H = new Node("testH", 100,100);
  H.setColor("H");
  graph.addNode(H);

  Node bnd = new Node("boundary", 100,200);
  bnd.setColor("boundary");
  graph.addNode(bnd);

  Node red = new Node("testR", 200,100);
  red.setColor("red");
  red.setAngle("\u03B1 + \u03B2");
  graph.addNode(red);

  Node green = new Node("testG", 200,200);
  green.setColor("green");
  green.setAngle("x");
  graph.addNode(green);

  backend = new QuantoBack(); 
}

void mousePressed() {
  Node n;
  Iterator it;
  switch (tool) {
    case 's':
      selectedNode = null;
      for (int i=0; i<graph.nodeList.size();++i) {
        n = (Node)graph.nodeList.get(i);
        n.selected = false;
        if (n.at(mouseX, mouseY)) {
          selectedNode = n;
          selectedIndex = i;
        }
      }
      if (selectedNode!=null) selectedNode.selected = true;
      break;
    case 'm':
      if (selectedNode != null) selectedNode.setDest(mouseX, mouseY);
      break;
      /* back end can;'t do edges yet....
      /*case 'e':
      it = graph.nodes.values().iterator();
      while (it.hasNext()) {
        n = (Node)it.next();
        if (n.at(mouseX, mouseY)) graph.newEdge(selectedNode, n);
      }
      graph.layoutGraph();
      break;
      */
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
  } else if (key == 'r' 
	     || key == 'g'
	     || key == 'h'
	     || key == 'b'
	     || key == 'n'
	     || key == 'u'
	     ) {
      /* all the commands the back end knows how to do we just pass on */
      backend.send(String.valueOf(key) + "\n");
      println(backend.receive());
      // here send D to backend and dump the graph
      // then rebuild it via the DOT parser.
      backend.send("D\n");
      graph = new Graph();
      layout(backend.receive(), graph);
      
      /*
    Node n = graph.newNode();
    if (key=='r') n.setColor("red");
    else n.setColor("green");
    if (selectedNode != null) graph.edges.add(new Edge(selectedNode, n));
    graph.layoutGraph();    
      */
  } else if (key == 'q') {
      println("Shutting down quantoML");
      backend.send("q\n");
      println(backend.receive());
      backend.send("quit () ; \n");
      println("Quitting....");
      exit();
  } else if (key==TAB) {
    if (graph.nodeList.size()>0) {
      selectedIndex = (selectedIndex+1)%graph.nodeList.size();
      if (selectedNode!=null) selectedNode.selected = false;
      selectedNode = (Node)graph.nodeList.get(selectedIndex);
      selectedNode.selected = true;
    }
  } else tool = key;
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
  for (int i=0; i<graph.edges.size(); ++i) {
    e = (Edge)graph.edges.get(i);
    e.display();
  }
  
  Node n;
  Iterator i = graph.nodes.values().iterator();
  while (i.hasNext()) {
    n = (Node)i.next();
    n.tick();
    n.display();
  }
  
}
