/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import java.awt.Color;
import java.awt.Component;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

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
	private JToggleButton enableButton;
	private JToggleButton disableButton;
	private JPopupMenu enableMenu = new JPopupMenu();
	private JPopupMenu disableMenu = new JPopupMenu();

	private void createMenus() {
		JMenuItem item = new JMenuItem("All");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					ruleset.activateAllRules();
				} catch (CoreException ex) {}
			}
		});
		enableMenu.add(item);
		item = new JMenuItem("Selection");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Object[] descs = listView.getSelectedValues();
					List<String> ruleNames = new LinkedList<String>();
					for (Object d : descs) {
						ruleNames.add(((RuleDescription)d).rulename);
					}
					ruleset.activateRules(ruleNames);
				} catch (CoreException ex) {}
			}
		});
		enableMenu.add(item);

		item = new JMenuItem("All");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					ruleset.deactivateAllRules();
				} catch (CoreException ex) {}
			}
		});
		disableMenu.add(item);
		item = new JMenuItem("Selection");
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Object[] descs = listView.getSelectedValues();
					List<String> ruleNames = new LinkedList<String>();
					for (Object d : descs) {
						ruleNames.add(((RuleDescription)d).rulename);
					}
					ruleset.deactivateRules(ruleNames);
				} catch (CoreException ex) {}
			}
		});
		disableMenu.add(item);
	}
	private void createMenuButtons() {
		enableButton = new JToggleButton("Enable");
		enableButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (enableButton.isSelected())
					enableMenu.show(RulesBar.this, enableButton.getX(), enableButton.getY() + enableButton.getHeight());
				else
					enableMenu.setVisible(false);
			}
		});
		enableMenu.addPopupMenuListener(new PopupMenuListener() {
			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {}
			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				enableButton.setSelected(false);
			}
			public void popupMenuCanceled(PopupMenuEvent e) {}
		});
		disableButton = new JToggleButton("Disable");
		disableButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (disableButton.isSelected())
					disableMenu.show(RulesBar.this, disableButton.getX(), disableButton.getY() + disableButton.getHeight());
				else
					disableMenu.setVisible(false);
			}
		});
		disableMenu.addPopupMenuListener(new PopupMenuListener() {
			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {}
			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				disableButton.setSelected(false);
			}
			public void popupMenuCanceled(PopupMenuEvent e) {}
		});
	}

	public RulesBar(Ruleset ruleset) {
		this.ruleset = ruleset;
		ruleset.addChangeListener(listener);

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

		createMenus();
		createMenuButtons();

		JButton refreshButton = new JButton("Refresh");
		refreshButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				RulesBar.this.ruleset.reload();
			}
		});

		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		this.add(enableButton);
		this.add(disableButton);
		this.add(listPane);
		this.add(refreshButton);

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
