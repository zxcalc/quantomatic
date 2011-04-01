/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import edu.uci.ics.jung.contrib.algorithms.layout.AbstractDotBangBoxLayout;
import quanto.core.data.BangBox;
import quanto.core.data.Edge;
import quanto.core.data.CoreGraph;
import quanto.core.data.Vertex;

/**
 *
 * @author alex
 */
public class QuantoDotLayout extends AbstractDotBangBoxLayout<Vertex, Edge, BangBox> {
	public static final double PADDING = 23.0;

	public QuantoDotLayout(CoreGraph graph) {
		super(graph, PADDING);
	}

	@Override
	protected String getVertexDotKey(Vertex vertex) {
		return vertex.getCoreName();
	}
}
