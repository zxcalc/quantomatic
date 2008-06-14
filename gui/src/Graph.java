import java.util.*;

public class Graph extends PLib {
	public Map<String,Vertex> vertices;
	public List<Vertex> vertexList;
	public Map<String,Edge> edges;
	public List<Edge> edgeList;
	private java.util.Random rando = new java.util.Random();
	public Vertex newestVertex;
	public static final int GRID_X = 25;
	public static final int GRID_Y = 25;

	public Graph() {
		vertices = new HashMap<String,Vertex>();
		vertexList = Collections.synchronizedList(new ArrayList<Vertex>());
		edges = new HashMap<String,Edge>();
		edgeList = Collections.synchronizedList(new ArrayList<Edge>());
		newestVertex = null;
	}

	
	// DEPRECATED
	// terrible fresh key generator -- not to be used
	private String freshKey(Map<String,?> m) {
		String fresh = String.valueOf(rando.nextInt());
		while (m.containsKey(fresh)) {
			fresh = String.valueOf(rando.nextInt());
		}
		return fresh;
	}

	// DEPRECATED -- don't call this
	// only the back-end is allowed to generate names
	public Vertex newVertex() {
		System.out.println("Warning: deprecated method newVertex called");
		Vertex n = new Vertex(freshKey(vertices), 50, 50);
		addVertex(n);
		return n;
	}

	public Vertex addVertex(Vertex n) {
		vertices.put(n.id, n);
		vertexList.add(n);
		return n;
	}

	// DEPRECATED -- don't call this
	// only the back-end is allowed to generate names
	public Edge newEdge(Vertex source, Vertex dest) {
		System.out.println("Warning: deprecated method newEdge called");
		Edge e = new Edge(freshKey(edges), source, dest);
		addEdge(e);
		return e;
	}

	public Edge addEdge(Edge e) {
		edges.put(e.id, e);
		edgeList.add(e);
		return e;
	}

	public void layoutGraph() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools
		
		p.layout(this);
	}

	/* this copies coordinates and selection info from the old graph to this one. */
	public Graph reconcileVertices(Graph old) {
		Vertex w = null;
		for (Vertex v : vertexList) {
			w = old.vertices.get(v.id);
			if (w != null) {
				v.x = w.x;
				v.y = w.y;
				v.selected = w.selected;
			} else {
				newestVertex = v;
			}
		}
		return this;
	}
	
	public void enableSnap() {
		synchronized(vertexList) {
			for (Vertex v : vertexList) {
				v.snap=true;
				v.setDest(v.x, v.y); // force the vertex to move to the snapped position
			}
		}
	}
	
	public void disableSnap() {
		synchronized(vertexList) {
			for (Vertex v : vertexList) v.snap=false;
		}
	}
	
	public void sortVertices() {
		synchronized(vertexList) {
			Collections.sort(vertexList);
		}
	}
	
	String toDot() {
		StringBuffer g = new StringBuffer();
		g.append("digraph { nodesep=0.65; ranksep=0.65;\n");

		for (Vertex v : vertexList) {
			g.append(v.id);
			g.append(" [color=\"");
			g.append(v.col);
			g.append("\",label=\"\",width=0.35,height=0.35,shape=circle];\n");
		}

		for (Edge e : edgeList) {
			g.append(e.source.id);
			g.append("->");
			g.append(e.dest.id);
			//g.append(" [arrowhead=none,headclip=false,tailclip=false];\n");
			g.append(" [arrowhead=none];\n");
		}

		g.append("\n}\n");
		return g.toString();
	}
	
	String toLatex() {
		if (vertexList.size()==0) return "\\begin{tikzpicture}\n\\end{tikzpicture}\n";
		StringBuffer g = new StringBuffer();
		g.append("\\begin{tikzpicture}\n");
		sortVertices();
		float xOrigin = vertexList.get(0).x;
		float yOrigin = vertexList.get(0).y;
		synchronized(vertexList) {
			for (Vertex v : vertexList) {
				g.append("    \\node ");
				if (v.col.equals("red")) {
					g.append("[rn] ");
				} else if (v.col.equals("green")) {
					g.append("[gn] ");
				} else if (v.col.equals("boundary")) {
					g.append("[dom] ");
				}
				g.append("(");
				g.append(v.id);
				g.append(") at (");
				g.append(Float.toString((v.x-xOrigin) / (float)GRID_X).replaceFirst(".0$", ""));
				g.append(", ");
				g.append(Float.toString((yOrigin-v.y) / (float)GRID_Y).replaceFirst(".0$", ""));
				g.append(") {};\n");
			}
		}
		
		g.append("\n    \\draw");
		int counter = 0;
		
		synchronized (edgeList) {
			for (Edge e : edgeList) {
				if (counter==4) {
					counter = 0;
					g.append("\n         ");
				}
				g.append(" (");
				g.append(e.source.id);
				g.append(")--(");
				g.append(e.dest.id);
				g.append(")");
				++counter;
			}
		}
		g.append(";");
		g.append("\n\\end{tikzpicture}\n");
		return g.toString();
	}

}