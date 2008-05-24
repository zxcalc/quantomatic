import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import javax.swing.JFileChooser;
import javax.swing.UIManager;

import processing.core.*;
import processing.xml.*;
import controlP5.ControlP5;

public class QuantoApplet extends PApplet {

	private static final long serialVersionUID = 1L;

	ControlP5 gui;
	PFont helvetica;
	PFont times;

	Vertex selectedVertex = null;
	int selectedIndex = -1;
	char tool;
	JFileChooser fileChooser;
	Graph graph;

	QuantoBack backend;
	XMLReader xml;
	static QuantoApplet p; // the top level applet 

	public void setup() {
		size(800, 600, JAVA2D);
		smooth();
		frameRate(30);
		gui = new ControlP5(this);
		gui.addTextlabel("sel", "SELECT", 10, 10).setColorValue(0xffff0000);
		gui.addTextlabel("mv", "MOVE", 10, 10).setColorValue(0xffff0000);
		gui.addTextlabel("ed", "EDGE", 10, 10).setColorValue(0xffff0000);

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
		Vertex H = new Vertex("testH", 100, 100);
		H.setColor("H");
		graph.addVertex(H);

		Vertex bnd = new Vertex("boundary", 100, 200);
		bnd.setColor("boundary");
		graph.addVertex(bnd);

		Vertex red = new Vertex("testR", 200, 100);
		red.setColor("red");
		red.setAngle("\u03B1 + \u03B2");
		graph.addVertex(red);

		Vertex green = new Vertex("testG", 200, 200);
		green.setColor("green");
		green.setAngle("x");
		graph.addVertex(green);

		backend = new QuantoBack();
		xml = new XMLReader();
		
		p = this;
	}

	public void mousePressed() {
		Vertex n;
		Iterator it;
		switch (tool) {
		case 's':
			selectedVertex = null;
			// IMPROVE: use tree of locations for sub-object matching:
			// get log-time search for finding object from coordinates instead
			// of linear time.
			for (int i = 0; i < graph.vertexList.size(); ++i) {
				n = (Vertex) graph.vertexList.get(i);
				n.selected = false;
				if (n.at(mouseX, mouseY)) {
					selectedVertex = n;
					selectedIndex = i;
				}
			}
			if (selectedVertex != null)
				selectedVertex.selected = true;
			break;
		case 'm':
			if (selectedVertex != null)
				selectedVertex.setDest(mouseX, mouseY);
			break;
		case 'e':
			if (selectedVertex != null) {
				it = graph.vertices.values().iterator();
				while (it.hasNext()) {
					n = (Vertex) it.next();
					if (n.at(mouseX, mouseY)) {
						modifyGraph("e " + selectedVertex.id + " " + n.id
								+ "\n");
					}
				}
			}
			break;
		}
	}

	public void keyPressed() {
		if (tool == key)
			tool = 's';
		else if (key == 'l')
			layout(graph);
		else if (key == 'o') {
			if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
				File f = fileChooser.getSelectedFile();
				String[] contents = loadStrings(f);
				StringBuffer accum = new StringBuffer();
				for (int j = 0; j < contents.length; ++j)
					accum.append(contents[j]);
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
			if (selectedVertex != null)
				modifyGraph("d " + selectedVertex.id);
		} else if (key == 'q') {
			println("Shutting down quantoML");
			backend.send("Q\n");
			println(backend.receive());
			backend.send("quit () ; \n");
			println("Quitting....");
			exit();
		} else if (key == TAB) {
			if (graph.vertexList.size() > 0) {
				selectedIndex = (selectedIndex + 1) % graph.vertexList.size();
				if (selectedVertex != null)
					selectedVertex.selected = false;
				selectedVertex = (Vertex) graph.vertexList.get(selectedIndex);
				selectedVertex.selected = true;
			}
		} else
			tool = key;
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

	public void draw() {
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
		for (int i = 0; i < graph.edgeList.size(); ++i) {
			e = (Edge) graph.edgeList.get(i);
			e.display();
		}

		Vertex n;
		Iterator i = graph.vertices.values().iterator();
		while (i.hasNext()) {
			n = (Vertex) i.next();
			n.tick();
			n.display();
		}

	}


	String makeDot(Graph graph) {
		StringBuffer g = new StringBuffer();
		g.append("digraph {\n\n");

		Iterator i = graph.vertices.values().iterator();
		Vertex n;
		while (i.hasNext()) {
			n = (Vertex) i.next();
			g.append("{rank=same; ");
			g.append(n.id);
			g.append(" [color=\"");
			g.append(n.col);
			g.append("\"];}\n");
		}

		i = graph.edgeList.iterator();
		Edge e;
		while (i.hasNext()) {
			e = (Edge) i.next();
			g.append(e.source.id);
			g.append("->");
			g.append(e.dest.id);
			g.append(";\n");
		}

		g.append("\n}\n");
		return g.toString();
	}

	void layout(Graph graph) {
		layout(makeDot(graph), graph);
	}

	void layout(String viz, Graph graph) {
		try {
			Process dot = Runtime.getRuntime().exec("dot -Tplain");
			BufferedReader dotIn = new BufferedReader(new InputStreamReader(dot
					.getInputStream()));

			OutputStreamWriter dotOut = new OutputStreamWriter(dot
					.getOutputStream());

			dotOut.write(viz);
			dotOut.close();

			println("NOW READING DOT");
			String ln = dotIn.readLine();
			StringTokenizer tk;
			String cmd, name;
			int x, y;
			Vertex n, n1, n2;
			graph.edges.clear();
			graph.edgeList.clear();
			while (!ln.equals("stop")) {
				println(ln);
				tk = new StringTokenizer(ln);
				cmd = tk.nextToken();
				if (cmd.equals("node")) {
					name = tk.nextToken();
					n = (Vertex) graph.vertices.get(name);
					if (n == null) {
						n = new Vertex(name, 50, 50);
						graph.addVertex(n);
					}
					x = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 20;
					y = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 20;

					tk.nextToken();
					tk.nextToken();
					tk.nextToken();
					tk.nextToken();
					tk.nextToken();
					// println(tk.nextToken());
					n.setColor(tk.nextToken());
					n.setDest(x, y);
				} else if (cmd.equals("edge")) {
					n1 = (Vertex) graph.vertices.get(tk.nextToken());
					n2 = (Vertex) graph.vertices.get(tk.nextToken());
					if (n1 == null || n2 == null) {
						println("Edge spec given before vertices defined.");
					} else {
						graph.newEdge(n1, n2);
					}
				}
				ln = dotIn.readLine();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		println("----NO MORE READING DOT----");
	}

}
