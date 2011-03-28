package edu.uci.ics.jung.contrib;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.util.*;

import org.apache.commons.collections15.SortedBag;
import org.apache.commons.collections15.bag.TreeBag;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.Graph;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AKDotLayout<V,E> extends AbstractLayout<V,E> {

	private final static Logger logger =
		LoggerFactory.getLogger(AKDotLayout.class);

	protected double vertexSpacing = 20.0;
	List<V> vertexTable;
	Map<V,Integer> inverseVertexTable;
	Graph<Integer,Integer> dag;
	Graph<Integer,Integer> tree;
	Graph<Integer,Integer> virtualGraph;
	Queue<Integer> edgeQueue;
	int edgeCounter;
	double width, height;
	int[] xBestCoords, yBestCoords;
	
	Map<Integer,Integer> ranks;
	int maxRank;
	Map<Integer,Integer> cutValues;
	List<Integer> edgesCopy;
	int badness;
	Ordering bestOrdering;
	int xBestLength;
	public int nodeSeparation;
	public int rankSeparation;
	private int coordPrecision;
	public static final int MAX_RANK_ITERATIONS = 10;
	public static final int CYCLE_SIZE = 5;
	
	public static final int MAX_ORDER_ITERATIONS = 8;
	public static final int MAX_POSITION_ITERATIONS = 16;
	
	public static final int OMEGA_RR = 1;
	public static final int OMEGA_VR = 2;
	public static final int OMEGA_VV = 8;

	public AKDotLayout(DirectedGraph<V, E> graph) {
		super(graph, new Dimension(600, 600));
	}

	public void initialize() {
		long tm = System.currentTimeMillis();
		
		nodeSeparation = 75;
		rankSeparation = 60;
		
		DirectedGraph<V,E> graph = (DirectedGraph<V,E>)getGraph();
		List<Set<V>> components = getComponentsForCut(graph, null);
		
		logger.debug("Processing {} component(s).", components.size());
		
		//List<AKDotLayout<V,E>> subLayouts = new ArrayList<AKDotLayout<V,E>>();
		double minX = (double)nodeSeparation;
		for (Set<V> comp : components) {
			DirectedGraph<V,E> gr = getSubgraphWithVertices(graph, comp);
			AKDotLayout<V,E> layout = new AKDotLayout<V,E>(gr);
			layout.rankSeparation = rankSeparation;
			layout.nodeSeparation = nodeSeparation;
			//subLayouts.add(layout);
			layout.doLayout();
			
			for (V v : comp) {
				Point2D p = layout.transform(v);
				p.setLocation(p.getX()+minX, p.getY()+(double)rankSeparation);
				setLocation(v, p);
			}
			
			minX += width + (double)nodeSeparation;
		}
		
		logger.debug("Layout took {} milliseconds",
				System.currentTimeMillis()-tm);
	}

	public void reset() {
		initialize();
	}

	@Override
	public Dimension getSize() {
		return new Dimension((int)Math.ceil(width), (int)Math.ceil(height));
	}

	@Override
	public void setSize(Dimension size) {
		throw new UnsupportedOperationException("Size of dot layout is determined by dot!");
	}
	
	public static <V,E> DirectedGraph<V, E>
	getSubgraphWithVertices(DirectedGraph<V, E> gr, Collection<V> verts) {
		DirectedGraph<V, E> sub = new DirectedSparseMultigraph<V, E>();
		
		for (V v : verts) sub.addVertex(v);
		
		for (E e : gr.getEdges()) {
			V s = gr.getSource(e);
			V d = gr.getDest(e);
			if (sub.containsVertex(s) && sub.containsVertex(d)) {
				sub.addEdge(e, s, d);
			}
		}
		
		return sub;
	}
	
	public static <V,E> List<Set<V>> getComponentsForCut(Graph<V, E> gr, E cut) {
		List<Set<V>> comps = new ArrayList<Set<V>>();
		Set<V> verts = new HashSet<V>(gr.getVertices());
		
		while (!verts.isEmpty()) {
			Set<V> comp = new HashSet<V>();
			addToComponent(verts.iterator().next(), gr, comp, cut);
			comps.add(comp);
			verts.removeAll(comp);
		}
		
		return comps;
	}
	
	public static <V,E> List<Set<V>> getComponents(Graph<V,E> gr) {
		return getComponentsForCut(gr, null);
	}
	
	public static <V,E> void addToComponent(V vert, Graph<V, E> gr, Set<V> comp, E cut) {
		comp.add(vert);
		for (E e : gr.getIncidentEdges(vert)) {
			if (e != cut) {
				V next = gr.getOpposite(vert, e);
				if (!comp.contains(next)) addToComponent(next, gr, comp, cut);
			}
		}
	}
	
	public static <Elem> Collection<Elem> trySortedSet(Collection<Elem> coll) {
		Collection<Elem> sorted;
		try {
			sorted = new TreeSet<Elem>(coll);
		} catch (ClassCastException e) {
			sorted = coll;
		}
		return sorted;
	}
	
	private void doLayout() {
		Graph<V, E> graph = getGraph();
		if (graph.getVertexCount()==0) return;
		logger.debug("Laying out graph ({}, {})", graph.getVertexCount(), graph.getEdgeCount());
		
		dagify();
		normalize(); // for debug purposes
		logger.debug("init ranks: {}", ranks);
		
		rank();
		logger.debug("final ranks: {}", ranks);
		logger.debug("max rank: {}", maxRank);
		
		ordering();
		position();
	}
	
	private void dagify() {
		dag = new DirectedSparseGraph<Integer, Integer>();
		ranks = new HashMap<Integer, Integer>();
		vertexTable = new ArrayList<V>(getGraph().getVertexCount());
		inverseVertexTable = new HashMap<V, Integer>();
		
		edgeCounter = 0;
		
		Collection<V> srt = trySortedSet(getGraph().getVertices());
		V root = srt.iterator().next();
		
		inverseVertexTable.put(root, 0);
		vertexTable.add(root);
		dag.addVertex(0);
		addToDag(root, 0, 0);
		
		logger.debug("dag ({}, {})", dag.getVertexCount(), dag.getEdgeCount());
		logger.debug("{}", dag);
	}
	
	private void addToDag(V vertex, int vertexId, int rnk) {
		Graph<V, E> graph = getGraph();
		ranks.put(vertexId, rnk);
		for (E in : trySortedSet(graph.getInEdges(vertex))) {
			V src = graph.getSource(in);
			if (inverseVertexTable.containsKey(src)) {
				int srcId = inverseVertexTable.get(src);
				if (srcId != vertexId &&
					dag.findEdge(srcId, vertexId)==null &&
					dag.findEdge(vertexId, srcId)==null)
				{ // merge multi-edges, ignore self-loops
					if (ranks.get(srcId) > rnk) {
						dag.addEdge(edgeCounter++, vertexId, srcId); // reverse edge
					} else {
						dag.addEdge(edgeCounter++, srcId, vertexId);
					}
				}
			} else {
				int srcId = vertexTable.size();
				inverseVertexTable.put(src, srcId);
				vertexTable.add(src);
				dag.addVertex(srcId);
				dag.addEdge(edgeCounter++, srcId, vertexId);
				addToDag(src, srcId, rnk-1);
			}
		}
		
		for (E out : trySortedSet(graph.getOutEdges(vertex))) {
			V dest = graph.getDest(out);
			if (inverseVertexTable.containsKey(dest)) {
				int destId = inverseVertexTable.get(dest);
				if (destId != vertexId && 
					dag.findEdge(vertexId, destId)==null &&
					dag.findEdge(destId, vertexId)==null)
				{ // merge multi-edges, ignore self-loops
					if (ranks.get(destId) < rnk) {
						dag.addEdge(edgeCounter++, destId, vertexId); // reverse edge
					} else {
						dag.addEdge(edgeCounter++, vertexId, destId);
					}
				}
			} else {
				int destId = vertexTable.size();
				inverseVertexTable.put(dest, destId);
				vertexTable.add(dest);
				dag.addVertex(destId);
				dag.addEdge(edgeCounter++, vertexId, destId);
				addToDag(dest, destId, rnk+1);
			}
//			if (dag.containsVertex(d)) {
//				if (dag.findEdge(v,d)==null) { // merge multi-edges
//					if (ranks.get(d) < rnk) {
//						dag.addEdge(out, d, v); // reverse edge
//					} else {
//						dag.addEdge(out, v, d);
//					}
//				}
//			} else {
//				dag.addVertex(d);
//				dag.addEdge(out, v, d);
//				addToDag(d, rnk+1);
//			}
		}
	}
	
	private void rank() {
		feasibleTree();
		logger.debug("init tree ({}, {}) (comps: {})",
			new Object[] {
			tree.getVertexCount(),
			tree.getEdgeCount(),
			getComponents(tree).size() });
		
		int iter = 0;
		int minBadness = Integer.MAX_VALUE;
		
		edgeQueue = new LinkedList<Integer>();
		for (int e : dag.getEdges()) {
			if (!tree.containsEdge(e)) edgeQueue.add(e);
		}
		
		while (updateCutValues()) {
			//logger.debug("Cut values: {}", cutValues);
			
			// anti-cycling: once iter hits MAX_ITERATIONS, watch for CYCLE_SIZE
			//   more iterations to find a local minimum. Try to break on that local
			//   min in the next CYCLE_SIZE iterations. Otherwise, break anyway.
			if (iter > MAX_RANK_ITERATIONS) {
				//badness = cutValues.size();
				if (iter < MAX_RANK_ITERATIONS+CYCLE_SIZE) {
					if (badness < minBadness) minBadness = badness;
				} else if (iter < MAX_RANK_ITERATIONS+CYCLE_SIZE+CYCLE_SIZE) {
					if (badness <= minBadness) break;
				} else {
					break;
				}
			}
			
			++iter;
		}
		logger.debug("rank(): cut values: {}", cutValues);

		logger.debug("final tree ({}, {}) (comps: {})",
			new Object[] {
			tree.getVertexCount(),
			tree.getEdgeCount(),
			getComponents(tree).size() });
		
		ranksFromFeasibleTree();
		normalize();
		balance();
	}
	
	private void feasibleTree() {
		initRank();
		
		Graph<Integer, Integer> graph = dag;
		int root = graph.getVertices().iterator().next();
		int numVerts = graph.getVertexCount();
		
		
		while (tightTree(root) < numVerts) {
			int minSlack = Integer.MAX_VALUE;
			boolean head, tail;
			boolean incidentHead = false;
			int src, dest;
			for (int e : graph.getEdges()) {
				if (!tree.containsEdge(e)) {
					src = graph.getSource(e);
					dest = graph.getDest(e);
					head = tree.containsVertex(dest);
					tail = tree.containsVertex(src);
					
					if (head != tail) {
						int slack =
							ranks.get(dest) -
							ranks.get(src) - 1;
						if (slack < minSlack) {
							minSlack = slack;
							incidentHead = head;
						}
					}
				}
			}
			
			int delta = (incidentHead) ? -minSlack : minSlack;
			
			for (int v : tree.getVertices()) {
				ranks.put(v, ranks.get(v) + delta);
			}
			
			//logger.debug("Ranks: {}", ranks);
		}
		
		//logger.debug("Tree: {}", tree);
	}
	
	private void initRank() {
		ranks = new HashMap<Integer, Integer>();
		
		Set<Integer> verts = new HashSet<Integer>(dag.getVertices());
		Set<Integer> marked = new HashSet<Integer>();
		
		int rnk = 0;
		
		// vertices marked for removal
		Collection<Integer> rem = new ArrayList<Integer>(verts.size());
		
		while (!verts.isEmpty()) {
			for (int v : verts) {
				boolean allMarked = true;
				for (int e : dag.getInEdges(v)) {
					if (!marked.contains(e)) {
						allMarked = false;
						break;
					}
				}
				
				if (allMarked) rem.add(v);
			}
			
			for (int v : rem) {
				for (int e : dag.getOutEdges(v)) marked.add(e);
				ranks.put(v, rnk);
				verts.remove(v);
			}
			
			++rnk;
			rem.clear();
		}
	}

	private int tightTree(int root) {
		tree = new DirectedSparseMultigraph<Integer, Integer>();
		tree.addVertex(root);
		tightTreeForVert(root);
		return tree.getVertexCount();
	}
	
	private void tightTreeForVert(int v) {
		for (int e : dag.getInEdges(v)) {
			int src = dag.getSource(e);
			if (!tree.containsVertex(src) &&
				(ranks.get(v) - ranks.get(src)) == 1)
			{
				tree.addVertex(src);
				tree.addEdge(e, src, v);
				tightTreeForVert(src);
			}
		}
		
		for (int e : dag.getOutEdges(v)) {
			int dest = dag.getDest(e);
			if (!tree.containsVertex(dest) &&
				(ranks.get(dest) - ranks.get(v)) == 1)
			{
				tree.addVertex(dest);
				tree.addEdge(e, v, dest);
				tightTreeForVert(dest);
			}
		}
	}
	
	private boolean updateCutValues() {
		cutValues = new HashMap<Integer, Integer>();
		Graph<Integer, Integer> graph = dag;
		
		int treeEdge = -1, outsideEdge = -1;
		int outsideSrc = -1, outsideDest = -1;
		
		badness = 0;
		
		for (int e : tree.getEdges()) {
			List<Set<Integer>> comps = getComponentsForCut(tree, e);
			int hd, tl;
			if (comps.get(0).contains(graph.getSource(e))) {
				tl = 0;
				hd = 1;
			} else {
				tl = 1;
				hd = 0;
			}
			
			int cut = 1;
			int minSlack = Integer.MAX_VALUE;
			int slack;
			//Collections.shuffle(edgesCopy);
			
			
			for (int e1 : edgeQueue) {
				int s = graph.getSource(e1);
				int d = graph.getDest(e1);
				if (comps.get(tl).contains(s) &&
					comps.get(hd).contains(d))
				{
					++cut;
				} else if (comps.get(tl).contains(d) &&
						   comps.get(hd).contains(s))
				{
					--cut;
					
					if (!tree.containsEdge(e1)) {
						slack = ranks.get(d) - ranks.get(s) - 1;
						if (slack < minSlack) {
							if (treeEdge == -1) {
								outsideEdge = e1;
								outsideSrc = s;
								outsideDest = d;
							}
							minSlack = slack;
						}
					}
				}
			}
			
			if (cut < 0) {
				if(treeEdge == -1) {
					treeEdge = e;
				}
				badness -= cut;
				cutValues.put(e, cut);
			}
			
			
		}
		
		if (treeEdge != -1 && outsideEdge != -1) {
			tree.removeEdge(treeEdge);
			edgeQueue.add(treeEdge);
			
			edgeQueue.remove(outsideEdge);
			tree.addEdge(outsideEdge, outsideSrc, outsideDest);
			return true;
		} else {
			return false;
		}
	}
	
	private void ranksFromFeasibleTree() {
		// use the feasible tree to reconstruct ranks
		ranks = new HashMap<Integer, Integer>();
		
		setRank(tree.getVertices().iterator().next(), 0);
	}
	
	private void setRank(int v, int rnk) {
		if (!ranks.containsKey(v)) {
			ranks.put(v, rnk);
			for (int in : tree.getInEdges(v))
				setRank(tree.getSource(in), rnk-1);
			
			for (int out : tree.getOutEdges(v))
				setRank(tree.getDest(out), rnk+1);
		}
	}

	private void normalize() {
		// normalize ranks
		int minRank = Integer.MAX_VALUE;
		maxRank = Integer.MIN_VALUE;
		for (int r : ranks.values()) {
			if (r < minRank) minRank = r;
			if (r > maxRank) maxRank = r;
		}
		
		if (minRank != 0) {
			maxRank -= minRank;
			for (int v : ranks.keySet()) {
				ranks.put(v, ranks.get(v)-minRank);
			}
		}
	}

	private void balance() {
		// TODO Auto-generated method stub
		
	}

	private void ordering() {
		initVirtualGraph();
		
		Ordering ord;
		
		ord = initOrdering(true);
		bestOrdering = new Ordering(ord);
		findBestOrdering(ord);
		
		ord = initOrdering(false);
		findBestOrdering(ord);
		
		for(int i = 0; i < 8; ++i) bestOrdering.transpose(i);
	}
	
	private void initVirtualGraph() {
		virtualGraph = new DirectedSparseGraph<Integer, Integer>();
		for (int v : dag.getVertices()) virtualGraph.addVertex(v);
		
		int vertexCounter = vertexTable.size();
		
		for (int e : dag.getEdges()) {
			int s = dag.getSource(e);
			int t = dag.getDest(e);
			int rs = ranks.get(s);
			int rt = ranks.get(t);
			if (rt - rs == 1) {
				virtualGraph.addEdge(e, s, t);
			} else {
				if (tree.containsEdge(e)) tree.removeEdge(e);
				
				for (int i = rs+1; i < rt; ++i) {
					virtualGraph.addVertex(vertexCounter);
					tree.addVertex(vertexCounter);
					ranks.put(vertexCounter, i);
					if (i==rs+1) {
						virtualGraph.addEdge(edgeCounter, s, vertexCounter);
						tree.addEdge(edgeCounter, s, vertexCounter);
					} else {
						virtualGraph.addEdge(edgeCounter, vertexCounter-1, vertexCounter);
						tree.addEdge(edgeCounter, vertexCounter-1, vertexCounter);
					}
					
					++edgeCounter;
					++vertexCounter;
				}
				
				virtualGraph.addEdge(edgeCounter++, vertexCounter-1, t);
				// if we don't add the final edge to the tree, it will always stay a tree
			}
		}
	}
	
	private void findBestOrdering(Ordering ord) {
		for (int i = 0; i < MAX_ORDER_ITERATIONS; ++i) {
			ord.wmedian(i);
			ord.transpose(i);
			ord.updateCrossings();
			logger.debug("findBestOrdering(): crossings: {}", ord.crossings);
			if (ord.crossings < bestOrdering.crossings) {
				bestOrdering = new Ordering(ord);
				logger.debug("best crossings: {}", bestOrdering.crossings);
			}
		}
	}
	
	private Ordering initOrdering(boolean direction) {
		Ordering ord = new Ordering(maxRank+1, virtualGraph);
		Queue<Integer> q = new LinkedList<Integer>();
		Collection<Integer> adjacent;
		for (Integer max : tree.getVertices()) {
			adjacent = (direction) ? tree.getInEdges(max) : tree.getOutEdges(max);
			if (adjacent.size() ==0) { // minimal (or maximal) vertex
				q.add(max);
				Integer v;
				while ((v = q.poll()) != null) {
					if (!ord.contains(v)) {
						ord.add(ranks.get(v), v);
						adjacent = (direction) ? tree.getOutEdges(v) : tree.getInEdges(v);
						
						for (Integer e : adjacent) {
							Integer v1 = tree.getOpposite(v, e);
							
							// note this check is performed when v1 is added _and_ removed
							if (!ord.contains(v1)) q.add(v1);
						}
					}
				}
			}
		}
		
		//orderTreeVertex(root, ord);
		ord.updateCrossings();
		
		return ord;
	}

//	private void orderTreeVertex(int v, Ordering ord) {
//		if (!ord.contains(v)) {
//			ord.add(ranks.get(v), v);
//			for (int v1 : tree.getNeighbors(v)) orderTreeVertex(v1, ord);
//		}
//	}
	
	private void position() {
		yCoordinate();
		xCoordinate();
		
		applyPosition();
	}
	
	private void yCoordinate() {
		yBestCoords = new int[bestOrdering.numRanks()];
		for (int i = 0; i < yBestCoords.length; ++i) {
			yBestCoords[i] = rankSeparation*i;
		}
		// TODO: tweak to fix slope abuse
	}

	private void xCoordinate() {
		int[] xCoords = initXCoord();
		
		coordPrecision = 4;
		
		for (int i = 0; i < xCoords.length; ++i) {
			xCoords[i] <<= coordPrecision;
		}
		
		xBestCoords = new int[xCoords.length];
		for (int i = 0; i < xBestCoords.length; ++i) xBestCoords[i] = xCoords[i];
		xBestLength = xLength(xBestCoords);
		int len;
		for (int i = 0; i < MAX_POSITION_ITERATIONS; ++i) {
			medianPos(i, xCoords);
			minEdge(i, xCoords);
			minNode(i, xCoords);
			minPath(i, xCoords);
			packCut(i, xCoords);
			
			len = xLength(xCoords);
			if (len <= xBestLength) {
				xBestLength = len;
				for (int j = 0; j < xCoords.length; ++j) {
					xBestCoords[j] = xCoords[j];
				}
			}
		}
		
		//snapToGrid();
		
		for (int i = 0; i < xCoords.length; ++i) {
			xBestCoords[i] >>= coordPrecision;
		}
		
		coordPrecision = 0;
		
		normaliseXCoords();
	}
	
//	private void snapToGrid() {
//		int gridsize = (nodeSeparation<<coordPrecision) / 2;
//		for (List<Integer> rank : bestOrdering.lists) {
//			int minpos = 0;
//			for (int v : rank) {
//				int pos = xBestCoords[v];
//				int gridBelow = (xBestCoords[v] / gridsize) * gridsize;
//				
//				pos = gridBelow;
//				
//				if (pos < minpos) pos = minpos;
//				xBestCoords[v] = pos;
//				minpos = pos + (nodeSeparation<<coordPrecision);
//			}
//		}
//	}

	private void normaliseXCoords() {
		int minPos = Integer.MAX_VALUE;
		int maxPos = Integer.MIN_VALUE;
		for (int c : xBestCoords) {
			if (c < minPos) minPos = c;
			if (c > maxPos) maxPos = c;
		}
		
		for (int i = 0; i < xBestCoords.length; ++i) {
			xBestCoords[i]-=minPos;
		}
		width = maxPos - minPos;
	}
	
	private int[] initXCoord() {
		int[] xCoords = new int[virtualGraph.getVertexCount()];
		
		int maxRankWidth = 0;
		for (List<Integer> rank : bestOrdering.lists) {
			if (rank.size() > maxRankWidth) maxRankWidth = rank.size();
		}
		
		width = (maxRankWidth-1) * (nodeSeparation);
		height = (bestOrdering.numRanks()-1) * (rankSeparation);
		
		for (int i = 0; i < bestOrdering.numRanks(); ++i) {
			List<Integer> rank = bestOrdering.lists[i];
			int offset = (maxRankWidth - rank.size()) * (nodeSeparation<<coordPrecision)/2;
			for (int j = 0; j < rank.size(); ++j) {
				xCoords[rank.get(j)] = offset + ((nodeSeparation<<coordPrecision) * j);
			}
		}
		
		return xCoords;
	}
	
	private int median(List<Integer> lst) {
		if (lst.size()%2 == 0) {
			return (lst.get(lst.size()/2-1) + lst.get(lst.size()/2))/2;
		} else {
			return lst.get(lst.size()/2);
		}
	}
	
	private int mean(List<Integer> lst) {
		int tot = 0;
		for (int val : lst) {
			tot += val;
		}
		return tot / lst.size();
	}
	
	private void medianPos(int iter, int[] coords) {
		int start, end, dir;
		if (iter % 4 < 2) {
			start = 0;
			end = bestOrdering.numRanks()-1;
			dir = 1;
		} else {
			start = bestOrdering.numRanks()-1;
			end = 0;
			dir = -1;
		}
		
		int nudgeSize = 4;
		
		for (int r = start; r * dir <= end * dir; r += dir) {
			List<Integer> rnk = bestOrdering.lists[r];
//			int totalOffset = 0;
//			int num = 0;
			
			int minPos;
			int maxPos;
			int mid;
			if (rnk.size() % 2 == 0) {
				mid = rnk.size()/2 - iter%2;
			} else {
				mid = rnk.size()/2;
			}
			
			// place the center node
			List<Integer> ch = new ArrayList<Integer>();
			int v = rnk.get(mid);
			for (int n : virtualGraph.getNeighbors(v))ch.add(coords[n]);
			Collections.sort(ch);
			
			int newPos;
			int nudge;
			int goal;
			if (!ch.isEmpty()) {
				goal = (iter%2==0) ? median(ch) : mean(ch);
				nudge = (goal - coords[v]) / nudgeSize;
				coords[v] += nudge;
			}
			
			
			maxPos = coords[v] - (nodeSeparation<<coordPrecision);
			minPos = coords[v] + (nodeSeparation<<coordPrecision);
			
			for (int i = 1; i < rnk.size()/2 + 1; ++i) {
				
				// place a node to the left of mid
				
				if (mid - i > 0) {
					v = rnk.get(mid - i);
					
					ch = new ArrayList<Integer>();
					for (int n : virtualGraph.getNeighbors(v))ch.add(coords[n]);
					Collections.sort(ch);
					
					newPos = coords[v];
					if (!ch.isEmpty()) {
						goal = (iter%2==0) ? median(ch) : mean(ch);
						nudge = (goal - coords[v]) / nudgeSize;
						newPos += nudge;
					}
					
					if (newPos > maxPos) newPos = maxPos;
					coords[v] = newPos;
					maxPos = newPos - (nodeSeparation<<coordPrecision);
				}
				
				// place a node to the right of mid
				
				if (mid + i < rnk.size()) {
					v = rnk.get(mid + i);
					
					ch = new ArrayList<Integer>();
					for (int n : virtualGraph.getNeighbors(v))ch.add(coords[n]);
					Collections.sort(ch);
					
					newPos = coords[v];
					if (!ch.isEmpty()) {
						goal = (iter%2==0) ? median(ch) : mean(ch);
						nudge = (goal - coords[v]) / nudgeSize;
						newPos += nudge;
					}
					
					if (newPos < minPos) newPos = minPos;
					coords[v] = newPos;
					minPos = newPos + (nodeSeparation<<coordPrecision);
				}
			}
		}
		
		// normalise, so no negative coords
//		int minCoord = Integer.MAX_VALUE;
//		for (int x : coords) if (x < minCoord) minCoord = x;
//		
//		if (minCoord != 0)
//			for (int i = 0; i < coords.length; ++i) coords[i] -= minCoord;
	}
	
	private void minEdge(int iter, int[] coords) {
		// TODO Auto-generated method stub
		
	}

	private void minNode(int iter, int[] coords) {
		// TODO Auto-generated method stub
		
	}

	private void minPath(int iter, int[] coords) {
		// TODO Auto-generated method stub
		
	}

	private void packCut(int iter, int[] coords) {
		// TODO Auto-generated method stub
		
	}
	
	private int xLength(int[] coords) {
		int s, d;
		int firstVirtual = vertexTable.size();
		int len = 0;
		int omega;
		for (int e : virtualGraph.getEdges()) {
			s = virtualGraph.getSource(e);
			d = virtualGraph.getDest(e);
			if (s < firstVirtual && d < firstVirtual) omega = OMEGA_RR;
			else if (s < firstVirtual != d < firstVirtual) omega = OMEGA_VR;
			else omega = OMEGA_VV;
			
			len += Math.abs(coords[d]-coords[s]) * omega;
//			int xdist = coords[d] - coords[s];
//			int ydist = (rankSeparation<<coordPrecision); 
//			len += omega * Math.sqrt(xdist*xdist + ydist*ydist);
		}
		
		return len;
	}
	
	private void applyPosition() {
		Point2D p;
		for (int i = 0; i < vertexTable.size(); ++i) {
			p = new Point2D.Double(xBestCoords[i], yBestCoords[ranks.get(i)]);
			setLocation(vertexTable.get(i), p);
		}
	}
	
	public static void main2(String[] args) {
		DirectedGraph<Integer,Integer> gr =
			new DirectedSparseMultigraph<Integer, Integer>();
		
		Random rand = new Random(156);
		int numVerts = 20;
		
		// random graph
		
		int numEdges = 30;
		
		for (int i = 0; i<numVerts; ++i) gr.addVertex(i);
		
		for (int i = numVerts; i < numVerts+numEdges; ++i) {
			gr.addEdge(i, 
					rand.nextInt(numVerts),
					rand.nextInt(numVerts));
		}
		
		
		// random (undirected) tree
		
		DirectedGraph<Integer,Integer> tr =
			new DirectedSparseMultigraph<Integer, Integer>();
		tr.addVertex(0);
		
		for (int i = 1; i < numVerts; ++i) {
			int src = (i==1) ? 0 : rand.nextInt(i-1);
			gr.addVertex(i);
			if (rand.nextBoolean()) {
				tr.addEdge(numVerts+i-1, src, i);
			} else {
				tr.addEdge(numVerts+i-1, i, src);
			}
		}
		
		tr.removeEdge(numVerts+7);
		
		System.out.println(gr);
		
		AKDotLayout<Integer,Integer> layout =
			new AKDotLayout<Integer, Integer>(gr);
		
		layout.initialize();
	}


	static class Ordering {
		public List<Integer>[] lists;
		public Map<Integer,Integer> indices;
		public Map<Integer,Integer> ranks;
		public int crossings;
		private Graph<Integer,Integer> graph;
		
		@SuppressWarnings("unchecked")
		public Ordering(int numRanks, Graph<Integer,Integer> graph) {
			this.graph = graph;
			crossings = 0;
			lists = new List/*<Integer>*/[numRanks];
			int rankSizeGuess = 2* (graph.getVertexCount() / numRanks);
			for (int i = 0; i<numRanks; ++i) {
				lists[i] = new ArrayList<Integer>(rankSizeGuess);
			}
			indices = new HashMap<Integer, Integer>();
			ranks = new HashMap<Integer,Integer>();
		}

		// deep copying constructor
		@SuppressWarnings("unchecked")
		public Ordering(Ordering ord) {
			graph = ord.graph;
			crossings = ord.crossings;
			
			lists = new List/*<Integer>*/[ord.lists.length];
			for (int i = 0; i < ord.lists.length; ++i) {
				lists[i] = (new ArrayList<Integer>(ord.lists[i]));
			}
			indices = new HashMap<Integer, Integer>(ord.indices);
			ranks = new HashMap<Integer, Integer>(ord.ranks);
		}
		
		public void add(int rnk, int v) {
			indices.put(v, lists[rnk].size());
			ranks.put(v, rnk);
			lists[rnk].add(v);
		}
		
		private void setVertex(int rnk, int pos, int v) {
			lists[rnk].set(pos, v);
			indices.put(v, pos);
		}
		
		private int getVertex(int rank, int j) {
			return lists[rank].get(j);
		}
		
		public boolean contains(int v) {
			return indices.keySet().contains(v);
		}
		
		public int numRanks() {
			return lists.length;
		}
		
		public String toString() {
			return "ordering: " + lists.toString();
		}
		
		public void updateCrossings() {
			int cr = 0;
			for (int i = 0; i<lists.length-1; ++i) {
				cr += crossingsForRank(i);
			}
			crossings = cr;
		}
		
		private int crossingsForRank(int rnk) {
			int[] edgesAfter = new int[lists[rnk+1].size()];
			int idx;
			int crossings = 0;
			SortedBag<Integer> markIdx = new TreeBag<Integer>();
			
			for (int i = 0; i<edgesAfter.length; ++i) edgesAfter[i] = 0;
			for (int v : lists[rnk]) {
				for (int n : graph.getNeighbors(v)) {
					if (ranks.get(n) == rnk+1) {
						idx = indices.get(n);
						crossings += edgesAfter[idx];
						markIdx.add(idx);
					}
				}
				
				for (int i = 0; i<edgesAfter.length; ++i) {
					while (markIdx.size() > 0 && markIdx.first() <= i) {
						markIdx.remove(markIdx.first());
					}
					edgesAfter[i] += markIdx.size();
				}
			}
			
			return crossings;
		}
		
		public void wmedian(int iter) {
			int dir, startRank, endRank;
			if (iter%2 == 0) {
				dir = 1;
				startRank = 1;
				endRank = lists.length-1;
			} else {
				dir = -1;
				startRank = lists.length-2;
				endRank = 0;
			}
			
			List<Integer> rnk;
			MedianEntry[] medians;
			for (int r = startRank; dir*r <= dir*endRank; r += dir) {
				rnk = lists[r];
				medians = new MedianEntry[rnk.size()];
				for (int j = 0; j < rnk.size(); ++j) {
					int vert = rnk.get(j);
					medians[j] = new MedianEntry(vert, medianValue(vert, r-dir), iter);
				}
				
				Arrays.sort(medians);
				for (int j = 0; j < rnk.size(); ++j) {
					setVertex(r, j, medians[j].vertex);
				}
			}
		}
		
		private static class MedianEntry implements Comparable<MedianEntry> {
			public int median;
			public int vertex;
			public int iteration;
			public MedianEntry(int vertex, int median, int iteration) {
				this.vertex = vertex;
				this.median = median;
				this.iteration = iteration;
			}
			public int compareTo(MedianEntry o) {
				if (median == -1 || o.median == -1) {
					return 0;
				} else {
					if (median > o.median) return 1;
					else if (median < o.median) return -1;
					else return 0;
				}
			}
		}
		
		// TODO: change to fixed point arithmetic for speed
		private int medianValue(int v, int adjRank) {
			List<Integer> pos = adjPositions(v, adjRank);
			int mid = pos.size()/2;
			if (pos.size() == 0) {
				return -1;
			} else if (pos.size() % 2 == 1) {
				return pos.get(mid);
			} else if (pos.size() == 2) {
				return (pos.get(0)+pos.get(1)) / 2;
			} else {
				int left = pos.get(mid-1)-pos.get(0);
				int right = pos.get(pos.size()-1) - pos.get(mid+1);
				return (pos.get(mid-1)*left + pos.get(mid)*right) / (left + right);
			}
		}

		private List<Integer> adjPositions(int v, int adjRank) {
			List<Integer> pos = new ArrayList<Integer>();
			Collection<Integer> neigh = graph.getNeighbors(v);
			for (int i = 0; i < lists[adjRank].size(); ++i) {
				if (neigh.contains(lists[adjRank].get(i))) pos.add(i);
			}
			return pos;
		}

		public void transpose(int iter) {
			int cross1, cross2;
			int tmp;
			boolean improved = true;
			
			int start, end, dir;
			
			if (iter % 4 < 2) {
				start = 0;
				end = numRanks()-1;
				dir = 1;
			} else {
				start = numRanks()-1;
				end = 0;
				dir = -1;
			}
			
			while (improved) {
				improved = false;
				for (int rank = start; dir*rank <= dir*end; rank+=dir) {
//					updateCrossings();
//					cross1 = crossings;
					cross1 = 0;
					if (rank>0) cross1 += crossingsForRank(rank-1);
					if (rank<numRanks()-1) cross1 += crossingsForRank(rank);
					
					for (int j = 0; j < lists[rank].size()-1; ++j) {
						tmp = getVertex(rank, j+1);
						setVertex(rank, j+1, getVertex(rank,j));
						setVertex(rank, j, tmp); 
						
//						updateCrossings();
//						cross2 = crossings;
						cross2 = 0;
						if (rank>0) cross2 += crossingsForRank(rank-1);
						if (rank<numRanks()-1) cross2 += crossingsForRank(rank);
						
						
						if (cross2 < cross1) {
							improved = true;
							cross1 = cross2;
						} else {
							tmp = getVertex(rank, j+1);
							setVertex(rank, j+1, getVertex(rank,j));
							setVertex(rank, j, tmp);
						}
					}
				}
			}
		}
	}
}
