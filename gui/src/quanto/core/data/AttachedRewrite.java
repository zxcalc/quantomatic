/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

/**
 *
 * @author alemer
 */
public class AttachedRewrite<G extends CoreGraph> implements Rewrite<G> {
	private G graph;
	private int index;
	private G lhs;
	private G rhs;
	private String ruleName;

	public AttachedRewrite(G graph, int index, String ruleName, G lhs, G rhs) {
		this.graph = graph;
		this.index = index;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	public G getGraph() {
		return graph;
	}

	public int getIndex() {
		return index;
	}
	
	public String getRuleName() {
		return ruleName;
	}

	public G getLhs() {
		return lhs;
	}

	public G getRhs() {
		return rhs;
	}
}
