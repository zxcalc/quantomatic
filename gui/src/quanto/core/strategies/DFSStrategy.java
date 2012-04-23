package quanto.core.strategies;

import java.util.List;

import quanto.core.Core;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;
import quanto.core.strategies.QuantoMetrics.comparisonType;

/*
 * Strategy based on metrics on graphs: Look n moves ahead and apply the rewrites that
 * minimize the metric after n rewrites
 */

public class DFSStrategy extends GenericStrategy implements QuantoStrategy {
	
	String strategyName = "DFSStrategy";
	private QuantoMetrics metrics = null;
	private int depth = 0;
	
	public DFSStrategy(Core core, QuantoMetrics metrics, int depth) {
		super(core);
		this.metrics = metrics;
		/* TODO for now we look look only one move ahead */
		this.depth = 1; //depth
	}
	
	@Override
	public int getNext(List<AttachedRewrite<CoreGraph>> rws, CoreGraph graph) {
		/*
		 * TODO: Since for now we just look one move ahead simply loop through all the
		 * rewrites and get the one that minimizes the score of the graph
		 */
		/* We have a list not a collection: we can rely on the order the elements */
		int min = -1;
		int count = 0;
		CoreGraph bestGraph = graph;
		for (AttachedRewrite<CoreGraph> rw : rws) {
			switch (this.metrics.compareGraphs(bestGraph, rw.getNewGraph())) {
				/* Unfortunately our comparison type forces to do that... */
				case GREATER:
					min = count;
					bestGraph = rw.getNewGraph();
				default: //Do nothing
			}
			count++;
		}
		if (min < 0)
			return 0;
		return min;
	}
}
