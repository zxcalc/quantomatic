package quanto;
import java.util.*;

public class Graph {
	Map<String,Vertex> vertices;
	Map<String,Edge> edges;
	
	public Vertex newestVertex;
	public static final int GRID_X = 25;
	public static final int GRID_Y = 25;
	protected GraphLayout layoutEngine;
	public CoordinateSystem coordinateSystem;

	public Graph(GraphLayout layoutEngine, CoordinateSystem cs) {
		vertices = new HashMap<String,Vertex>();
		edges = new HashMap<String,Edge>();
		newestVertex = null;
		this.layoutEngine = layoutEngine;
		this.coordinateSystem = cs;
	}
	
	public Graph() {
		this(new DotLayout(), CoordinateSystem.IDENTITY);
	}
	
	public Map<String,Vertex> getVertices() {return vertices;} 
	public Map<String,Edge> getEdges() {return edges;};
	
//	// DEPRECATED -- don't call this
//	// only the back-end is allowed to generate names
//	public Vertex newVertex() {
//		System.out.println("Warning: deprecated method newVertex called");
//		Vertex n = new Vertex(GlobalNameSpace.newName().toString(), 50, 50);
//		addVertex(n);
//		return n;
//	}
	
	public void addVertex(Vertex n) {
		n.setGraph(this);
		vertices.put(n.id, n);
	}

//	// DEPRECATED -- don't call this
//	// only the back-end is allowed to generate names
//	public Edge newEdge(Vertex source, Vertex dest) {
//		System.out.println("Warning: deprecated method newEdge called");
//		Edge e = new Edge(GlobalNameSpace.newName().toString(), source, dest);
//		addEdge(e);
//		return e;
//	}

	public void addEdge(Edge e) {
		e.setGraph(this);
		edges.put(e.id, e);
	}

	public void layoutGraph() {
		layoutEngine.layout(this);
		recomputeEdgeOffsets();
	}

	/* copies vertices and edges from newg over the existing vertices and edges */
	public void updateTo(Graph newg) {
		//this.coordinateSystem.print();
		synchronized(vertices) {
			Map<String,Vertex> oldvertices = vertices;
			vertices = newg.getVertices();
			edges = newg.getEdges();

			Vertex w = null;
			for (Vertex v : vertices.values()) {
				v.setGraph(this);
				w = oldvertices.get(v.id);
				if (w == null) {
					newestVertex = v;
					v.x = v.destX = QuantoApplet.WIDTH / 2
							+ (float) Math.random() * 50.0f - 25.0f;
					v.y = v.destY = QuantoApplet.HEIGHT / 2
							+ (float) Math.random() * 50.0f - 25.0f;
				} else {
					v.x = v.destX = w.x;
					v.y = v.destY = w.y;
					v.selected = w.selected;
				}
			}
		}
		synchronized(edges) {
			for (Edge e : edges.values()) {
				e.setGraph(this);
			}
		}
	}
	
	public void deselectAllVertices() {
		for (Vertex v : getVertices().values()) v.selected = false;
	}

	public void enableSnap() {
		synchronized(vertices) {
			for (Vertex v : vertices.values()) {
				v.snap=true;
				v.setDest(v.x, v.y); // force the vertex to move to the snapped position
			}
		}
	}
	
	public void disableSnap() {
		synchronized(vertices) {
			for (Vertex v : vertices.values()) v.snap=false;
		}
	}
	
	public void shift(float dx, float dy) {
		for(Vertex v : vertices.values()){
			v.shift(dx,dy);
		}
	}

	public List<Edge> getParallelEdges(Vertex v, Vertex w) {
		ArrayList<Edge> bag = new ArrayList<Edge>();
		for(Edge e : v.edges) {
			if((e.source == w && e.dest == v)||(e.source == v && e.dest == w)) bag.add(e);
		}
		return bag;
	}
	
	private void updateEdgeOffsets(Vertex v, Vertex w) {
		updateEdgeOffsets(getParallelEdges(v,w));
	}
	
	private void updateEdgeOffsets(Edge edge) {
		updateEdgeOffsets(edge.dest, edge.source);
	}
	
	private void updateEdgeOffsets(List<Edge> parallel) {

		if(parallel.size() > 0) {
			Vertex v = parallel.get(0).source;

			if(parallel.size() %2 == 1){ // odd number
				parallel.remove(0).setOffset(0);
			}
			int count = 0;
			for(Edge e : parallel) {
				// if edges are anti-parallel the offset is reversed
				int dir = (e.source == v) ? 1 : -1;
				if(count%2 == 0) {
					e.setOffset(dir*(count/2 + 1));
				}
				else {
					e.setOffset(-(dir*(count+1)/2));
				}
				count++;
			}
		}
	}

	private void recomputeEdgeOffsets(Collection<Vertex> vcol) {
		if(!vcol.isEmpty()) {
			List<Vertex> vlist = new ArrayList<Vertex>(vcol);
			Vertex v = vlist.remove(0);
			updateSelfLoopOffsets(v);
			for(Vertex w : vlist) {
				updateEdgeOffsets(v,w);
			}
			recomputeEdgeOffsets(vlist);
		}
	}
	
	private void recomputeEdgeOffsets() {
		recomputeEdgeOffsets(vertices.values());
	}
	
	private void updateSelfLoopOffsets(Vertex v) {
		List<Edge> elist = getParallelEdges(v,v);
		int count = 0;
		for(Edge e : elist) {
			count++;
			e.setOffset(count);
		}
	}

	/** this won't work with curved edges as it only computes
	 * based on the vertices
	 * @return
	 */
	public BoundingBox getBoundingBox() {
		float maxX=0, maxY=0, minX=0, minY=0;
		if(!getVertices().isEmpty()) {
			// initialise accumulators
			Vertex first = getVertices().values().iterator().next();
			minX = maxX = first.x;
			minY = maxY = first.y;
			for(Vertex v : getVertices().values()) {
				maxX = maxX < v.x ? v.x : maxX;
				minX = minX > v.x ? v.x : minX;
				maxY = maxY < v.y ? v.y : maxY;
				minY = minY > v.y ? v.y : minY;
			}
			maxX += Vertex.radius + 1;
			maxY += Vertex.radius +1 ;
			minX -= Vertex.radius + 1;
			minY -= Vertex.radius +1 ;
		}
		return new BoundingBox(minX,minY,maxX,maxY);	
		
	}
	
	public BoundingBox getBoundingBoxAtDest() {
		float maxX=0, maxY=0, minX=0, minY=0;
		if(!getVertices().isEmpty()) {
			// initialise accumulators
			Vertex first = getVertices().values().iterator().next();
			minX = maxX = first.destX;
			minY = maxY = first.destY;
			for(Vertex v : getVertices().values()) {
				maxX = maxX < v.destX ? v.destX : maxX;
				minX = minX > v.destX ? v.destX : minX;
				maxY = maxY < v.destY ? v.destY : maxY;
				minY = minY > v.destY ? v.destY : minY;
			}
			maxX += Vertex.radius + 1;
			maxY += Vertex.radius +1 ;
			minX -= Vertex.radius + 1;
			minY -= Vertex.radius +1 ;
		}
		return new BoundingBox(minX,minY,maxX,maxY);			
	}
	
	/** returns a vertex overlapping with the point (x,y).  If there are several overlapping points it choose one 
	 * aribtrarily; if there are none, it returns null;
	 * @param x
	 * @param y
	 * @return
	 */
	public Vertex getVertexAtPoint(float x, float y, int coordType) {
		for(Vertex v: getVertices().values()){
			if(v.at(x, y, coordType)) return v;
		}
		return null;
	}

	
	public String toDot() {
		StringBuffer g = new StringBuffer();
		g.append("digraph { nodesep=0.65; ranksep=0.65;\n");

		for (Vertex v : vertices.values()) {
			g.append(v.id);
			g.append(" [color=\"");
			g.append(v.col);
			g.append("\",label=\"\",width=0.35,height=0.35,shape=circle];\n");
		}

		for (Edge e : edges.values()) {
			g.append(e.source.id);
			g.append("->");
			g.append(e.dest.id);
			//g.append(" [arrowhead=none,headclip=false,tailclip=false];\n");
			g.append(" [arrowhead=none];\n");
		}
		g.append("\n}\n");
		return g.toString();
	}
	
	public String toLatex() {
		if (vertices.size()==0) return "\\begin{tikzpicture}\n\\end{tikzpicture}\n";
		StringBuffer g = new StringBuffer();
		g.append("\\begin{tikzpicture}\n");
		
		Vertex origin = vertices.values().iterator().next();
		float xOrigin = origin.x;
		float yOrigin = origin.y;
		synchronized(vertices) {
			for (Vertex v : vertices.values()) {
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
		
		synchronized (edges) {
			for (Edge e : edges.values()) {
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

	public GraphLayout getLayoutEngine() {
		return layoutEngine;
	}

	public void setLayoutEngine(GraphLayout layoutEngine) {
		this.layoutEngine = layoutEngine;
	}

	public boolean tick() {
		boolean moved = false;
		for (Vertex v : vertices.values()) {
			moved = moved || v.tick();
			v.tick();
			v.display();
		}
		return moved;
	}

}