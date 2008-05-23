import java.util.Map;
import java.util.HashMap;

class Graph {
    public Map vertices;
    public ArrayList vertexList;
    public Map edges;
    public ArrayList edgeList;
    private java.util.Random rando = new java.util.Random();
  
    public Graph() {
	vertices = new HashMap();
	vertexList = new ArrayList();
	edges = new HashMap();
	edgeList = new ArrayList();
    }
  
    // DEPRECATED
    // terrible fresh key generator -- not to be used
    private String freshKey(Map m) {
	String fresh = String.valueOf(rando.nextInt());
	while (m.containsKey(fresh)) {
	    fresh = String.valueOf(rando.nextInt());
	}
	return fresh;
    }

    // DEPRECATED -- don't call this 
    // only the backend is allowed to generate names
    public Vertex newVertex() {
	println("Warning: deprecated method newVertex called");
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
    // only the backend is allowed to generate names
    public Edge newEdge(Vertex source, Vertex dest) {
	println("Warning: deprecated method newEdge called");
	Edge e = new Edge(freshKey(edges),source, dest);
	addEdge(e);
	return e;
    }

    public Edge addEdge(Edge e) {
	edges.put(e.id, e);
	edgeList.add(e);
	return e;
    }

    public void layoutGraph() {
	layout(this);
    }
    
    /* this copies the x and y from the old graph to this one. */
    public Graph reconcileVertexCoords(Graph old) {
	Iterator i = vertexList.iterator();
	Vertex v = null, w = null;
	while(i.hasNext()) {
	    v = (Vertex)i.next();
	    w = (Vertex)old.vertices.get(v.id);
	    if(w != null) {
		v.x = w.x;
		v.y = w.y;
	    }
	}
	return this;
    }

}
