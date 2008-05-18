import java.io.*;
import java.util.Map;
import java.util.HashMap;
import java.util.StringTokenizer;

String makeDot(Graph graph) {
  StringBuffer g = new StringBuffer();
  g.append("digraph {\n\n");
  
  Iterator i = graph.nodes.values().iterator();
  Node n;
  while (i.hasNext()) {
    n = (Node)i.next();
    g.append("{rank=same; ");
    g.append(n.id);
    g.append(" [color=\"");
    g.append(n.col);
    g.append("\"];}\n");
  }
  
  i = graph.edges.iterator();
  Edge e;
  while (i.hasNext()) {
    e = (Edge)i.next();
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
  BufferedReader dotIn =
    new BufferedReader(
    new InputStreamReader(
      dot.getInputStream()
    ));
  		
  OutputStreamWriter dotOut = 
    new OutputStreamWriter(
      dot.getOutputStream()
    );
		
  dotOut.write(viz);
  dotOut.close();
		
  String ln = dotIn.readLine();
  StringTokenizer tk;
  String cmd, name;
  int x, y;
  Node n, n1, n2;
  graph.edges.clear();
  while (!ln.equals("stop")) {
    tk = new StringTokenizer(ln);
    cmd = tk.nextToken();
    if (cmd.equals("node")) {
      name = tk.nextToken();
      n = (Node)graph.nodes.get(name);
      if (n == null) {
        n = new Node(name, 50, 50);
        graph.addNode(n);
      }
      x = (int)(Float.parseFloat(tk.nextToken())*50.0)+20;
      y = (int)(Float.parseFloat(tk.nextToken())*50.0)+20;
      
      tk.nextToken();tk.nextToken();tk.nextToken();tk.nextToken();tk.nextToken();
      //println(tk.nextToken());
      n.setColor(tk.nextToken());
      n.setDest(x,y);
    } else if (cmd.equals("edge")) {
      n1 = (Node)graph.nodes.get(tk.nextToken());
      n2 = (Node)graph.nodes.get(tk.nextToken());
      if (n1==null || n2==null) {
        println("Edge spec given before nodes defined.");
      } else {
        graph.newEdge(n1, n2);
      }
    }
    ln = dotIn.readLine();
  }
  } catch (IOException e) {
    e.printStackTrace();
  }
}
