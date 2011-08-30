package quanto.gui;

import edu.uci.ics.jung.contrib.algorithms.layout.AbstractForceLayout;
import quanto.core.data.Edge;
import quanto.core.data.CoreGraph;
import quanto.core.data.Vertex;

public class QuantoForceLayout  extends AbstractForceLayout<Vertex, Edge> {
		public static final double PADDING = 23.0;

		public QuantoForceLayout(CoreGraph graph) {
			super(graph, PADDING);
		}
	
}
