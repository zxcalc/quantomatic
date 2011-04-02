/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import javax.swing.JPanel;
import quanto.core.Ruleset;

/**
 * Panel displaying a very simple rules interface.
 *
 * This just lists the rules, allowing them to be filtered
 * by tag, and enables/disables rules (individually, all at
 * once, or by tag).
 *
 * @author alex
 */
public class RulesBar extends JPanel {
	private Ruleset ruleset;

	public RulesBar(Ruleset ruleset) {
		this.ruleset = ruleset;
	}
}
