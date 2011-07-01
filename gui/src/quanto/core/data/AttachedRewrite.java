/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

/**
 *
 * @author alemer
 */
public class AttachedRewrite<G extends CoreGraph> {
	private G graph;
	private int index;
    private Rule<G> rule;
	private G newGraph;

	public AttachedRewrite(G graph, int index, Rule<G> rule, G newGraph) {
		this.graph = graph;
		this.index = index;
		this.rule = rule;
        this.newGraph = newGraph;
	}

	public G getGraph() {
		return graph;
	}

	public int getIndex() {
		return index;
	}
	
	public String getRuleName() {
		return rule.getCoreName();
	}

	public G getLhs() {
		return rule.getLhs();
	}

	public G getRhs() {
		return rule.getRhs();
	}

	public G getNewGraph() {
		return newGraph;
	}
}
