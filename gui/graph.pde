import java.util.Map;
import java.util.HashMap;

class Graph {
  public Map nodes;
  public ArrayList nodeList;
  public ArrayList edges;
  
  public Graph() {
    nodes = new HashMap();
    edges = new ArrayList();
    nodeList = new ArrayList();
  }
  
  public Node newNode() {
    Iterator i = nodes.values().iterator();
    Node n;
    int id, maxId=0;
    while (i.hasNext()) {
      n = (Node)i.next();
      try {
        id = (int)Integer.parseInt(n.id);
        maxId = max(id, maxId);
      } catch (NumberFormatException e) {}
    }
    
    n = new Node(Integer.toString(maxId+1), 50, 50);
    addNode(n);
    return n;
  }
  
  public Node addNode(Node n) {
    nodes.put(n.id, n);
    nodeList.add(n);
    return n;
  }
  
  public Edge newEdge(Node source, Node dest) {
    Edge e = new Edge(source, dest);
    edges.add(e);
    return e;
  }
  
  public void layoutGraph() {
    layout(this);
  }
}
