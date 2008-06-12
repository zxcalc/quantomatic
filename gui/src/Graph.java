import java.util.*;

public class Graph extends PLib {
	public Map<String,Vertex> vertices;
	public List<Vertex> vertexList;
	public Map<String,Edge> edges;
	public List<Edge> edgeList;
	private java.util.Random rando = new java.util.Random();
	public Vertex newestVertex;

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

	/* this copies coords and selection info from the old graph to this one. */
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
	
	public void sortVertices() {
		Collections.sort(vertexList);
	}

}