package edu.uci.ics.jung.contrib;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.io.*;
import java.util.*;


import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.Graph;

public class DotLayout<V extends HasName,E> extends AbstractLayout<V,E> {
	public static class DotException extends RuntimeException {
		private static final long serialVersionUID = 8173148319748759814L;

		public DotException(String s) {
			super(s);
		}
	}
	
	public DotLayout(Graph<V,E> graph, Dimension size) {
		super(graph,size);
	}
	
	public DotLayout(Graph<V,E> graph) {
		super(graph);
	}

	public void initialize() {
		try {
			synchronized (getGraph()) {
				if (getGraph().getVertexCount() == 0) return;
				Map<String,V> verts = new HashMap<String,V>();
				for (V v : getGraph().getVertices()) verts.put(v.getName(),v);
				
				String viz = graphToDot();
				Process dot = Runtime.getRuntime().exec("dot -Tplain");
				BufferedReader dotIn = new BufferedReader(new InputStreamReader(dot
						.getInputStream()));
	
				OutputStreamWriter dotOut = new OutputStreamWriter(dot
						.getOutputStream());
	
				dotOut.write(viz);
				dotOut.close();
	
				String ln = dotIn.readLine();
				
				// compute bounds as we go
				double bottom=0, right=0;
				while (!ln.equals("stop")) {
					StringTokenizer tk = new StringTokenizer(ln);
					String cmd = tk.nextToken();
					if (cmd.equals("node")) {
						String name = tk.nextToken();
						Point2D loc = new Point2D.Double(
								(Double.parseDouble(tk.nextToken()) * 50.0),
								(Double.parseDouble(tk.nextToken()) * 50.0)
							);
						if (loc.getX()>right) right = loc.getX();
						if (loc.getY()>bottom) bottom = loc.getY();
						setLocation(verts.get(name), loc);
					}
					ln = dotIn.readLine();
				}
				
				// center to graph in the provided space
				double shiftX = (getSize().getWidth() - right) / 2.0;
				double shiftY = (getSize().getHeight() - bottom) / 2.0;
				for (V v : getGraph().getVertices()) {
					offsetVertex(v, shiftX, shiftY);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void reset() {
		initialize();
	}

	
	/*
	 * Converts a graph to a DOT string. ALWAYS run in the context
	 * of synchronized(getGraph()) {...}.
	 */
	private String graphToDot() {
		StringBuffer g = new StringBuffer();
		g.append("digraph { nodesep=0.65; ranksep=0.65;\n");

		for (V v : getGraph().getVertices()) {
			g.append(v.getName());
			g.append(" [label=\"\",width=0.35,height=0.35,shape=circle];\n");
		}

		for (E e : getGraph().getEdges()) {
			g.append(getGraph().getSource(e).getName());
			g.append("->");
			g.append(getGraph().getDest(e).getName());
			g.append(" [arrowhead=none];\n");
		}
		g.append("\n}\n");
		//System.out.println(g.toString());
		return g.toString();
	}
}
