/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.graph.DirectedGraph;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author alex
 */
public class DotLayout<V,E> extends AbstractDotLayout<V, E> {
	private Map<V,String> vertexKeys = null;

	public DotLayout(DirectedGraph<V,E> graph, double vertexPadding) {
		super(graph, vertexPadding);
	}

	@Override
	protected void beginLayout() {
		vertexKeys = new HashMap<V, String>();
		int i = 1;
		for (V v : graph.getVertices()) {
			vertexKeys.put(v, Integer.toString(i));
			++i;
		}
	}

	@Override
	protected String getVertexDotKey(V vertex) {
		return vertexKeys.get(vertex);
	}

	@Override
	protected void endLayout() {
		vertexKeys = null;
	}
}
