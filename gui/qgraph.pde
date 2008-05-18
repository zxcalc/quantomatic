import controlP5.*;
import javax.swing.*;

ControlP5 gui;
Textlabel selectLabel;
Textlabel moveLabel;

Node selectedNode=null;
int selectedIndex=-1;
char tool;
JFileChooser fileChooser;
Graph graph;

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
  
  graph = new Graph();
  tool = 's';
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
    case 'e':
      it = graph.nodes.values().iterator();
      while (it.hasNext()) {
        n = (Node)it.next();
        if (n.at(mouseX, mouseY)) graph.newEdge(selectedNode, n);
      }
      graph.layoutGraph();
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
  } else if (key == 'r' || key == 'g') {
    Node n = graph.newNode();
    if (key=='r') n.setColor("red");
    else n.setColor("green");
    if (selectedNode != null) graph.edges.add(new Edge(selectedNode, n));
    graph.layoutGraph();    
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
