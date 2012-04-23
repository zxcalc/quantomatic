package quanto.core.strategies;

import java.util.List;

import quanto.core.Core;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

//Generic strategy: always apply the first rewrite

public class GenericStrategy implements QuantoStrategy {

	public String strategyName = "GenericStrategy";
	private Core core = null;
	
	public GenericStrategy(Core core) {
		this.core = core;
	}
	
	public int getNext(List<AttachedRewrite<CoreGraph>> rws, CoreGraph graph) {
		//Always apply the 1st rewrite: equivalent to the lexical order
		return 0;
	}

	public String getName() {
		return this.strategyName;
	}
}
