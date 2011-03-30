/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.contrib.graph.DirectedBangBoxGraph;
import java.util.Collection;

/**
 *
 * @author alemer
 */
public abstract class AbstractDotBangBoxLayout<V, E, B> extends AbstractDotLayout<V, E> {


	public AbstractDotBangBoxLayout(DirectedBangBoxGraph<V,E,B> graph, double vertexSpacing) {
		super(graph, vertexSpacing);
	}

	@Override
	protected boolean isWorkToDo() {
		return super.isWorkToDo() || (getGraph().getBangBoxCount() > 0);
	}

	@Override
	@SuppressWarnings("unchecked")
	public DirectedBangBoxGraph<V, E, B> getGraph() {
		return (DirectedBangBoxGraph<V, E, B>)super.getGraph();
	}

	protected void addBangBoxLines(StringBuilder g) {
		int i = 0;
		for (B b : getGraph().getBangBoxes()) {
			Collection<V> contents = getGraph().getBoxedVertices(b);
			if (!contents.isEmpty()) {
				g.append("subgraph \"cluster ");
				g.append(i);
				g.append("\" {\n");
				for (V v : getGraph().getBoxedVertices(b)) {
					g.append("\"");
					g.append(getVertexDotKey(v));
					g.append("\"; ");
				}
			}
			g.append("\n}\n");
		}
	}

	@Override
	protected void addGraphContents(StringBuilder g) {
		addVertexLines(g);
		addEdgeLines(g);
		addBangBoxLines(g);
	}
}
