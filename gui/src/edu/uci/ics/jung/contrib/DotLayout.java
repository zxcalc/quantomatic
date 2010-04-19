package edu.uci.ics.jung.contrib;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.io.*;
import java.util.*;


import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.DirectedGraph;

public class DotLayout<V extends HasName,E> extends AbstractLayout<V,E> {
	public static String dotProgram = "dot";
	
	
	public static class DotException extends RuntimeException {
		private static final long serialVersionUID = 8173148319748759814L;

		public DotException(String s) {
			super(s);
		}
	}
	
	public DotLayout(DirectedGraph<V,E> graph, Dimension size) {
		super(graph,size);
	}
	
	public DotLayout(DirectedGraph<V,E> graph) {
		super(graph);
	}

	/**
	 * (Re-)initialize the layout.
	 */
	public void initialize() {
		try {
			synchronized (getGraph()) {
				if (getGraph().getVertexCount() == 0) return;
				
				String viz = "digraph {\n" + graphToDot() + "\n}\n\n";
				Map<String,Point2D> coords = getCoordMap(viz);
				
				for (V v : getGraph().getVertices()) {
					setLocation(v, coords.get("layoutvert_" + v.getName()));
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	protected Map<String,Point2D> getCoordMap(String viz) throws IOException {
		Map<String,Point2D> coords = new HashMap<String, Point2D>();
		Process dot = Runtime.getRuntime().exec(dotProgram + " -Tplain");
		BufferedReader dotIn = new BufferedReader(new InputStreamReader(dot
				.getInputStream()));

		OutputStreamWriter dotOut = new OutputStreamWriter(dot
				.getOutputStream());

		dotOut.write(viz);
		dotOut.close();

		String ln = dotIn.readLine();
		
		// compute bounds as we go
		double top=0;//, right=0;
		while (!ln.equals("stop")) {
			StringTokenizer tk = new StringTokenizer(ln);
			String cmd = tk.nextToken();
			if (cmd.equals("node")) {
				String name = tk.nextToken();
				Point2D loc = new Point2D.Double(
						(Double.parseDouble(tk.nextToken()) * 50.0 + 30.0),
						(Double.parseDouble(tk.nextToken()) * -50.0)
					);
				//if (loc.getX()>right) right = loc.getX();
				if (loc.getY()<top) top = loc.getY();
				coords.put(name, loc);
			}
			ln = dotIn.readLine();
		}
		
		for (Point2D c : coords.values()) {
			c.setLocation(c.getX(), c.getY()-top+40.0);
		}
		
		return coords;
	}
	
	public void reset() {
		initialize();
	}

	
	/**
	 * Converts a graph to a DOT string. ALWAYS run in the context
	 * of synchronized(getGraph()) {...}.
	 */
	protected String graphToDot() {
		StringBuffer g = new StringBuffer();
		g.append("nodesep=0.65; ranksep=0.65;\n");

		for (V v : getGraph().getVertices()) {
			g.append("layoutvert_");
			g.append(v.getName());
			g.append(" [label=\"");
			g.append(v.toString());
			g.append("\",width=0.35,height=0.35,shape=rectangle];\n");
		}

		for (E e : getGraph().getEdges()) {
			g.append("layoutvert_");
			g.append(getGraph().getSource(e).getName());
			g.append("->");
			g.append("layoutvert_");
			g.append(getGraph().getDest(e).getName());
			g.append(" [arrowhead=none];\n");
		}
		//System.out.println(g.toString());
		return g.toString();
	}
}
