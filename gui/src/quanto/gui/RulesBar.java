/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import java.awt.Color;
import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.core.CoreException;
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

	private final static Logger logger =
		LoggerFactory.getLogger(RulesBar.class);

	private static class RuleDescription
	{
		public RuleDescription(String rulename, boolean active) {
			this.rulename = rulename;
			this.active = active;
		}
		public String rulename;
		public boolean active;

		public String toString() {
			return rulename;
		}
	};

	private Ruleset ruleset;
	private ChangeListener listener = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				loadRules();
			}
		};
	private JList listView;
	private DefaultListModel rulesModel;

	public RulesBar(Ruleset ruleset) {
		this.ruleset = ruleset;
		ruleset.addChangeListener(listener);

		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

		DefaultListCellRenderer cellRenderer = new DefaultListCellRenderer() {
			public Component getListCellRendererComponent(
					JList list,
					Object value,
					int index,
					boolean isSelected,
					boolean cellHasFocus) {
				super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
				if (!((RuleDescription)value).active) {
					setForeground(Color.gray);
				}
				return this;
			}
		};

		rulesModel = new DefaultListModel();
		listView = new JList(rulesModel);
		listView.setCellRenderer(cellRenderer);
		JScrollPane listPane = new JScrollPane(listView);
		this.add(listPane);
		loadRules();
	}

	private void loadRules() {
		rulesModel.clear();
		try {
			for (String rule : ruleset.getRules()) {
				rulesModel.addElement(new RuleDescription(rule, ruleset.isRuleActive(rule)));
			}
		} catch (CoreException ex) {
			logger.error("Could not get rules", ex);
		}
	}
}
