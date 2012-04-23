package quanto.core.strategies;

import quanto.core.data.CoreGraph;

/*
 * Simple Metrics based on the number of vertices and the number of edges.
 * If two graphs have the same number of vertices, then look at the number of edges.
 */

public class SimpleMetrics implements QuantoMetrics {

	String metricsName = "SimpleMetrics";
	
	public SimpleMetrics() {}
	
	public int getScore(CoreGraph graph) {
		/* Simply return the number of vertices */
		return graph.getVertexCount();
	}

	public comparisonType compareGraphs(CoreGraph g1, CoreGraph g2) {
		if (g1.getVertexCount() > g2.getVertexCount())
			return comparisonType.GREATER;
		else if (g1.getVertexCount() < g2.getVertexCount())
			return comparisonType.SMALLER;
		else { /* total order here */
			if (g1.getEdgeCount() > g2.getEdgeCount())
				return comparisonType.GREATER;
			else if (g1.getEdgeCount() < g2.getEdgeCount())
				return comparisonType.SMALLER;
			else
				return comparisonType.EQUAL; /* Let the strategy make a choice */
		}
	}

	public String getName() {
		return this.metricsName;
	}
	
}
