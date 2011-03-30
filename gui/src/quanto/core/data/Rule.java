/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

/**
 *
 * @author alemer
 */
public class Rule<G extends CoreGraph> implements Rewrite<G>, CoreObject {
	private String name;
	private G lhs;
	private G rhs;

	public Rule(String name, G lhs, G rhs) {
		this.name = name;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public G getLhs() {
		return lhs;
	}

	public G getRhs() {
		return rhs;
	}
}
