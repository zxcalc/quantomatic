package quanto.core.strategies;

import quanto.core.data.CoreGraph;

/*
 * Metrics take a graph an return a score which should represent
 * a given feature.
 * NOTE: Good metrics should take into account vertex types etc...
 * and therefore are theory specific. The definitions of such metrics
 * should be implemented in the core along with the GraphicalTheory: A metric can
 * have access to the core.
 */

public interface QuantoMetrics {
	
	String metricsName = null;
	
	public enum comparisonType {
		GREATER, 
		SMALLER, 
		EQUAL, 
		NONE /*Could be a partial order...*/
	};
	
	public String getName();
	public int getScore(CoreGraph graph);
	
	public comparisonType compareGraphs(CoreGraph g1, CoreGraph g2);
}
