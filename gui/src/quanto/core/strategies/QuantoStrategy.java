package quanto.core.strategies;

import java.util.List;

import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

/*
 * Gui strategies decide in which order the rules should be applied.
 * They have access to the core but should *not* apply the rules. They return
 */

public interface QuantoStrategy {
	final String strategyName = null;
	
	int getNext(List<AttachedRewrite<CoreGraph>> rws, CoreGraph graph);
	String getName();
}
