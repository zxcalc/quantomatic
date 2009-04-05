package edu.uci.ics.jung.contrib;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;

/**
 * EdgeIndexFunction that draws parallel edges in a balanced way. Note
 * this returns indices in [-n/2, n/2], rather that [0, n-1].
 * @author aleks
 *
 */
public class BalancedEdgeIndexFunction<V,E> implements EdgeIndexFunction<V,E> {
	private Map<E,Integer> indexer;
	private BalancedEdgeIndexFunction() {
		indexer = new HashMap<E,Integer>();
	}
	
	public static <V,E>BalancedEdgeIndexFunction<V,E> getInstance() {
		return new BalancedEdgeIndexFunction<V,E>();
	}
	
	public int getIndex(Graph<V, E> graph, E e) {
		Integer idx = indexer.get(e);
		if (idx==null) {
			reset(graph, e);
			idx = indexer.get(e);
		}
		return idx;
	}

	public void reset() {
		indexer = new HashMap<E,Integer>();
	}
	
	public void reset(Graph<V, E> g, E edge) {
		synchronized (g) {
			V v1 = g.getSource(edge);
			V v2 = g.getDest(edge);
			List<E> out = new ArrayList<E>();
			List<E> in = new ArrayList<E>();
			
			// Use two loops to make sure one direction comes
			// before the other.
			for (E e : g.getOutEdges(v1)) {
				if (g.getDest(e)==v2) {
					out.add(e);
				}
			}
			
			for (E e : g.getInEdges(v1)) {
				if (g.getSource(e)==v2) {
					in.add(e);
				}
			}
			
			int index;
			if (v1==v2) {
				index = 0;
				for (E e : in) indexer.put(e, index++);
				for (E e : out) indexer.put(e, index++);
			} else {
				int edge_count = in.size()+out.size();
				index = -edge_count/2 - 1;
				for (E e : in) {
					indexer.put(e,index);
					index++;
					if (index == -1 && edge_count%2==0) index++;
				}
				index+=2;
				for (E e : out) {
					// these edges are drawn in the opposite orientation,
					//  so bend backwards.
					indexer.put(e,-index);
					index++;
					if (index == 1 && edge_count%2==0) index++;
				}
			}
		}
		
	}
}
