package quanto.gui;

import java.awt.geom.Point2D;
import java.io.*;
import java.util.*;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.Graph;

public class DotLayout<V extends NamedVertex,E> extends AbstractLayout<V,E> {
	public static class DotException extends RuntimeException {
		private static final long serialVersionUID = 8173148319748759814L;

		public DotException(String s) {
			super(s);
		}
	}
	
	public DotLayout(Graph<V,E> graph) {
		super(graph);
	}

	public void initialize() {
		try {
			synchronized (getGraph()) {
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
				
				while (!ln.equals("stop")) {
					StringTokenizer tk = new StringTokenizer(ln);
					String cmd = tk.nextToken();
					if (cmd.equals("node")) {
						String name = tk.nextToken();
						Point2D loc = new Point2D.Double(
								(Double.parseDouble(tk.nextToken()) * 50.0) + 43,
								(Double.parseDouble(tk.nextToken()) * 50.0) + 43
							);
						setLocation(verts.get(name), loc);
					}
					ln = dotIn.readLine();
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
