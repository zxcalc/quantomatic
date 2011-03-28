/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author alex
 */
public class DotBangBoxLayout<V,E,B> extends AbstractBangBoxDotLayout<V,E,B> {
	private Map<V,String> vertexKeys = null;
	private Map<B,String> bangBoxKeys = null;

	public DotBangBoxLayout(DirectedBangBoxGraph<V,E,B> graph, double vertexPadding, double bangBoxPadding) {
		super(graph, vertexPadding, bangBoxPadding);
	}

	@Override
	protected void beginLayout() {
		vertexKeys = new HashMap<V, String>();
		int i = 1;
		for (V v : graph.getVertices()) {
			vertexKeys.put(v, Integer.toString(i));
			++i;
		}
		bangBoxKeys = new HashMap<B, String>();
		for (B b : ((DirectedBangBoxGraph<V, E, B>)graph).getBangBoxes()) {
			bangBoxKeys.put(b, Integer.toString(i));
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
		bangBoxKeys = null;
	}

	@Override
	protected String getBangBoxDotKey(B bangbox) {
		return bangBoxKeys.get(bangbox);
	}
}
