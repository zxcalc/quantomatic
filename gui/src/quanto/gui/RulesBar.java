/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
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

	private static class RuleDescription {

		public RuleDescription(String rulename, boolean active) {
			this.rulename = rulename;
			this.active = active;
		}
		public String rulename;
		public boolean active;

		@Override
		public String toString() {
			return rulename;
		}
	};
	private Ruleset ruleset;
	private ChangeListener listener = new ChangeListener() {

		public void stateChanged(ChangeEvent e) {
			loadRules((String) tagsCombo.getSelectedItem());
		}
	};
	private JList listView;
	private DefaultListModel rulesModel;
	private JToggleButton enableButton;
	private JToggleButton disableButton;
	private JComboBox tagsCombo;
	private JPopupMenu enableMenu = new JPopupMenu();
	private JPopupMenu disableMenu = new JPopupMenu();

	private JMenuItem enableAllJMenuItem  = new JMenuItem("All");
	private JMenuItem disableAllJMenuItem  = new JMenuItem("All");
	private JMenuItem enableSelectionJMenuItem  = new JMenuItem("Selection");
	private JMenuItem disableSelectionJMenuItem  = new JMenuItem("Selection");

	private void createMenus() {
		enableAllJMenuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					if (tagsCombo.getSelectedIndex() != 0) {
						ruleset.activateRulesByTag((String) tagsCombo.getSelectedItem());
					}
					else {
						ruleset.activateAllRules();
					}
				}
				catch (CoreException ex) {
				}
			}
		});
		enableMenu.add(enableAllJMenuItem);
		enableSelectionJMenuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					Object[] descs = listView.getSelectedValues();
					List<String> ruleNames = new LinkedList<String>();
					for (Object d : descs) {
						ruleNames.add(((RuleDescription) d).rulename);
					}
					ruleset.activateRules(ruleNames);
				}
				catch (CoreException ex) {
				}
			}
		});
		enableMenu.add(enableSelectionJMenuItem);

		disableAllJMenuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					if (tagsCombo.getSelectedIndex() != 0) {
						ruleset.deactivateRulesByTag((String) tagsCombo.getSelectedItem());
					}
					else {
						ruleset.deactivateAllRules();
					}
				}
				catch (CoreException ex) {
				}
			}
		});
		disableMenu.add(disableAllJMenuItem);
		disableSelectionJMenuItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					Object[] descs = listView.getSelectedValues();
					List<String> ruleNames = new LinkedList<String>();
					for (Object d : descs) {
						ruleNames.add(((RuleDescription) d).rulename);
					}
					ruleset.deactivateRules(ruleNames);
				}
				catch (CoreException ex) {
				}
			}
		});
		disableMenu.add(disableSelectionJMenuItem);
	}

	protected ImageIcon createImageIcon(String path,
					    String description) {
		java.net.URL imgURL = getClass().getResource(path);
		if (imgURL != null) {
			return new ImageIcon(imgURL, description);
		}
		else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}

	private void createMenuButtons() {
		enableButton = new JToggleButton(createImageIcon("/toolbarButtonGraphics/quanto/ComputeAdd16.gif", "Enable"));
		enableButton.setToolTipText("Enable rules");
		enableButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if (enableButton.isSelected()) {
					enableMenu.show(RulesBar.this, enableButton.getX(), enableButton.getY() + enableButton.getHeight());
				}
				else {
					enableMenu.setVisible(false);
				}
			}
		});
		enableMenu.addPopupMenuListener(new PopupMenuListener() {

			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
			}

			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				enableButton.setSelected(false);
			}

			public void popupMenuCanceled(PopupMenuEvent e) {
			}
		});
		disableButton = new JToggleButton(createImageIcon("/toolbarButtonGraphics/quanto/ComputeRemove16.gif", "Disable"));
		disableButton.setToolTipText("Disable rules");
		disableButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if (disableButton.isSelected()) {
					disableMenu.show(RulesBar.this, disableButton.getX(), disableButton.getY() + disableButton.getHeight());
				}
				else {
					disableMenu.setVisible(false);
				}
			}
		});
		disableMenu.addPopupMenuListener(new PopupMenuListener() {

			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
			}

			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
				disableButton.setSelected(false);
			}

			public void popupMenuCanceled(PopupMenuEvent e) {
			}
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
				if (!((RuleDescription) value).active) {
					setForeground(Color.gray);
				}
				return this;
			}
		};

		rulesModel = new DefaultListModel();
		listView = new JList(rulesModel);
		listView.setCellRenderer(cellRenderer);
		JScrollPane listPane = new JScrollPane(listView);
		tagsCombo = new JComboBox();
		createMenus();
		createMenuButtons();

		JButton refreshButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/Refresh16.gif", "Refresh"));
		refreshButton.setToolTipText("Refresh rules");
		refreshButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				RulesBar.this.ruleset.reload();
			}
		});
		
		tagsCombo.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent e) {
				JComboBox cb = (JComboBox)e.getSource();
		        String tag = (String)cb.getSelectedItem();
		        loadRules(tag);
			}
		});
		
		JPanel buttonBox = new JPanel();
		buttonBox.setLayout(new BoxLayout(buttonBox, BoxLayout.LINE_AXIS));
		buttonBox.add(enableButton);
		buttonBox.add(disableButton);
		buttonBox.add(refreshButton);
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		this.add(buttonBox);
		this.add(tagsCombo);
		/*Because of the BoxLayout, the Jcombobox takes too much space
		width = width of the listPane, height = preferred height*/
		tagsCombo.setMaximumSize(new Dimension((int)listPane.getPreferredSize().getWidth(), (int)tagsCombo.getPreferredSize().getHeight()));
		this.add(listPane);

		loadTags();
		loadRules("All Rules");
	}

	private void loadTags() {
		try {
			tagsCombo.removeAllItems();
			tagsCombo.addItem("All Rules");
			for(String tag : ruleset.getTags()){
				tagsCombo.addItem(tag);
			}
		}
		catch (CoreException ex) {
			logger.error("Could not get tags", ex);
		}
	}
	
	private void loadRules(String tag) {
		rulesModel.clear();
		/* If the tag exists, load the corresponding rules.
		   If not then load all the rules.*/
		try {
			if (tagsCombo.getSelectedIndex() != 0) {
				//Switch from "All" to "Tag" accordingly
				enableAllJMenuItem.setText("Tag");
				disableAllJMenuItem.setText("Tag");

				for (String rule : ruleset.getRulesByTag(tag)) {
					rulesModel.addElement(new RuleDescription(rule, ruleset.isRuleActive(rule)));
				}
			}
			else {
				enableAllJMenuItem.setText("All");
				disableAllJMenuItem.setText("All");

				for (String rule : ruleset.getRules()) {
					rulesModel.addElement(new RuleDescription(rule, ruleset.isRuleActive(rule)));
				}
			}
		}
		catch (CoreException ex) {
			logger.error("Could not get rules and/or tags", ex);
		}
	}
}
