import java.util.Map;
import java.util.HashMap;

class Graph {
  public Map vertices;
  public ArrayList vertexList;
  public ArrayList edges;
  
  public Graph() {
    vertices = new HashMap();
    edges = new ArrayList();
    vertexList = new ArrayList();
  }
  
  public Vertex newVertex() {
    Iterator i = vertices.values().iterator();
    Vertex n;
    int id, maxId=0;
    while (i.hasNext()) {
      n = (Vertex)i.next();
      try {
        id = (int)Integer.parseInt(n.id);
        maxId = max(id, maxId);
      } catch (NumberFormatException e) {}
    }
    
    n = new Vertex(Integer.toString(maxId+1), 50, 50);
    addVertex(n);
    return n;
  }
  
  public Vertex addVertex(Vertex n) {
    vertices.put(n.id, n);
    vertexList.add(n);
    return n;
  }
  
  public Edge newEdge(Vertex source, Vertex dest) {
    Edge e = new Edge(source, dest);
    edges.add(e);
    return e;
  }
  
  public void layoutGraph() {
    layout(this);
  }
}
