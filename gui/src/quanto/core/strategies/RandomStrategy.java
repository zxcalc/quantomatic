package quanto.core.strategies;

import java.util.List;
import java.util.Random;

import quanto.core.Core;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

/* 
 * Example of a strategy: return a random int
 */

public class RandomStrategy extends GenericStrategy implements QuantoStrategy {

	String strategyName = "RandomStrategy";
	private Random r = null;
	
	public RandomStrategy(Core core) {
		super(core);
		this.r = new Random();
	}
	
	@Override
	public int getNext(List<AttachedRewrite<CoreGraph>> rws, CoreGraph graph) {
		return r.nextInt(rws.size());
	}
}
