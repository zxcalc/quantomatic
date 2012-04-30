package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.io.*;
import java.util.*;


import edu.uci.ics.jung.graph.DirectedGraph;
import java.util.logging.Level;
import java.util.logging.Logger;

public abstract class AbstractDotLayout<V,E> extends AbstractLayout<V, E> implements DynamicBoundsLayout {

private final static Logger logger =
Logger.getLogger("edu.uci.ics.jung.contrib.algorithms.layout");

public static String dotProgram = "dot";
public final static double DOT_SCALE = 50.0;

protected Map<String,Point2D> vertexPositions = null;
protected double vertexSpacing = 20.0;

public AbstractDotLayout(DirectedGraph<V,E> graph, double vertexSpacing) {
super(graph, new Dimension((int)Math.ceil(2*vertexSpacing), (int)Math.ceil(2*vertexSpacing)));
this.vertexSpacing = vertexSpacing;
}

protected void beginLayout() {}

protected void endLayout() {}

/**
* Get the vertex key to use to describe a vertex.
*
* This must be unique (within the graph) for each vertex, and
* must always return the same value for any given vertex between a
* call to beginLayout() and the corresponding call to endLayout().
*
* @return a string containing no double quote characters
*/
protected abstract String getVertexDotKey(V vertex);

@Override
public Dimension getSize() {
return size;
}

@Override
public void setSize(Dimension size) {
throw new UnsupportedOperationException("Size of dot layout is determined by dot!");
}

public void recalculateSize() {
double right = vertexSpacing;
double bottom = vertexSpacing;
for (V v : getGraph().getVertices()) {
Point2D point = transform(v);
right = Math.max(right, point.getX() + vertexWidth(v)/2.0);
bottom = Math.max(bottom, point.getY() + vertexHeight(v)/2.0);
}
right += vertexSpacing;
bottom += vertexSpacing;
size.setSize(Math.ceil(right), Math.ceil(bottom));
}

@Override
public void setLocation(V picked, Point2D p) {
if (p.getX() < 20)
p.setLocation(20, p.getY());
if (p.getY() < 20)
p.setLocation(p.getX(), 20);
super.setLocation(picked, p);
if (p.getX() + vertexSpacing > size.width) {
size.width = (int)Math.ceil(p.getX() + vertexSpacing);
}
if (p.getY() + vertexSpacing > size.height) {
size.height = (int)Math.ceil(p.getY() + vertexSpacing);
}
}

protected boolean isWorkToDo() {
return getGraph().getVertexCount() > 0;
}

/**
* (Re-)initialize the layout.
*/
public void initialize() {
try {
synchronized (getGraph()) {
if (!isWorkToDo()) return;

vertexPositions = new HashMap<String, Point2D>();
beginLayout();

String viz = graphToDot();
calculateNodePositions(viz);
adjustPositions();
layoutGraph();

endLayout();
//recalculateSize();
}
} catch (IOException e) {
logger.log(Level.SEVERE, "Failed to run dot", e);
} finally {
vertexPositions = null;
}
}

public void reset() {
initialize();
}

protected void calculateNodePositions(String dot) throws IOException {
Process dotProcess = Runtime.getRuntime().exec(dotProgram + " -Tplain");
BufferedReader dotIn = new BufferedReader(new InputStreamReader(dotProcess
.getInputStream()));

OutputStreamWriter dotOut = new OutputStreamWriter(dotProcess
.getOutputStream());

dotOut.write(dot);
dotOut.close();

String ln = dotIn.readLine();

while (!ln.equals("stop")) {
logger.log(Level.FINEST, "Processing line: {0}", ln);
if (ln.startsWith("graph ")) {
StringTokenizer tok = new StringTokenizer(ln);
tok.nextToken(); // "graph"
tok.nextToken(); // scale
double width = Double.parseDouble(tok.nextToken());
double height = Double.parseDouble(tok.nextToken());
size.setSize(width * DOT_SCALE + 2*vertexSpacing, height * DOT_SCALE + 2*vertexSpacing);
}
if (ln.startsWith("node ")) {
// ad-hoc parsing, as we know exactly what the
// format will be
StringBuilder tok = new StringBuilder();
int p = "node ".length();
if (ln.charAt(p) == '"') {
++p;
while (ln.charAt(p) != '"') {
tok.append(ln.charAt(p));
++p;
}
++p;
} else {
while (ln.charAt(p) != ' ') {
tok.append(ln.charAt(p));
++p;
}
}
String name = tok.toString();
++p;

tok = new StringBuilder();
while (ln.charAt(p) != ' ') {
tok.append(ln.charAt(p));
++p;
}
double x = Double.parseDouble(tok.toString()) * DOT_SCALE + vertexSpacing;
++p;

tok = new StringBuilder();
while (ln.charAt(p) != ' ') {
tok.append(ln.charAt(p));
++p;
}
double y = Double.parseDouble(tok.toString()) * DOT_SCALE + vertexSpacing;
y = size.height - y;

vertexPositions.put(name, new Point2D.Double(x, y));
}
ln = dotIn.readLine();
if (ln == null) {
throw new DotException("Bad dot output: no 'stop' received");
}
}

dotIn.close();
}

protected void adjustPositions() {
/*double top=0;
for (Point2D c : vertexPositions.values()) {
c.setLocation(c.getX() * DOT_SCALE + vertexSpacing, c.getY() * -DOT_SCALE);
if (c.getY()<top)
top = c.getY();
}

logger.log(Level.FINEST,
"Top is {0}, adjusting all Y values by {1}",
new Object[] {top, vertexSpacing-top});
for (Point2D c : vertexPositions.values()) {
c.setLocation(c.getX(), c.getY()-top+vertexSpacing);
}*/

}

protected void layoutGraph() {
for (V v : graph.getVertices()) {
if (!isLocked(v))
setLocationNoUpdates(v, vertexPositions.get(getVertexDotKey(v)));
}
}

protected void addGraphAttrs(StringBuilder g) {
g.append("nodesep=");
g.append(vertexSpacing/DOT_SCALE);
g.append("; ranksep=");
g.append(vertexSpacing/DOT_SCALE);
g.append("; ");

}

protected double vertexWidth(V v) { return 14; }
protected double vertexHeight(V v) { return 14; }

protected void addVertexLines(StringBuilder g) {
for (V v : graph.getVertices()) {
g.append("\"");
g.append(getVertexDotKey(v));
g.append("\" [width=");
g.append(vertexWidth(v)/DOT_SCALE);
g.append(",height=");
g.append(vertexHeight(v)/DOT_SCALE);
g.append(",shape=rectangle,fixedsize=yes];\n");
}
}

protected void addEdgeLines(StringBuilder g) {
for (E e : getGraph().getEdges()) {
g.append("\"");
g.append(getVertexDotKey(graph.getSource(e)));
g.append("\"->\"");
g.append(getVertexDotKey(graph.getDest(e)));
g.append("\"");
g.append(" [arrowhead=none];\n");
}
}

protected void addGraphContents(StringBuilder g) {
addVertexLines(g);
addEdgeLines(g);
}

/**
* Converts a graph to a DOT string. ALWAYS run in the context
* of synchronized(getGraph()) {...}.
*/
protected String graphToDot() {
StringBuilder g = new StringBuilder();
g.append("digraph {\n");
addGraphAttrs(g);
g.append("\n");
addGraphContents(g);
g.append("}\n");
logger.log(Level.FINEST, "Dot output: {0}", g);
return g.toString();
}

/**
* Sets the location without updating bang box rects.
*
* For use in layoutGraph()
*
* @param picked
* @param p
*/
protected void setLocationNoUpdates(V picked, Point2D p) {
super.setLocation(picked, p);
}

/**
* Sets the location without updating bang box rects.
*
* For use in layoutGraph()
*
* @param picked
* @param p
*/
protected void setLocationNoUpdates(V picked, double x, double y) {
super.setLocation(picked, x, y);
}
}
