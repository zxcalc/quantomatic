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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import quanto.core.CoreException;
import quanto.core.Ruleset;
import quanto.core.data.CoreGraph;
import quanto.core.data.Rule;

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
		Logger.getLogger("quanto.gui");

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
	private ViewPort viewPort;
	private ChangeListener listener = new ChangeListener() {
		public void stateChanged(ChangeEvent e) {
            loadTags();
			loadRules((String) tagsCombo.getSelectedItem());
		}
	};
	private JList listView;
	private DefaultListModel rulesModel;
	private JButton enableButton;
	private JButton disableButton;
	private JButton deleteButton;
	private JButton createRuleButton;
	private JButton refreshButton;
	private JComboBox tagsCombo;
	
	private boolean suppressTagComboCallback = false;

	private JPopupMenu createRuleContextualMenu() {
		JPopupMenu popupMenu = new JPopupMenu();
		JMenuItem menuItem = new JMenuItem("Edit rule");
		popupMenu.add(menuItem);
		menuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editRule(listView.getSelectedValue().toString());
			}
		});
		
		menuItem = new JMenuItem("Delete rule");
          popupMenu.add(menuItem);
          menuItem.addActionListener(new ActionListener() {
               public void actionPerformed(ActionEvent e) {
                    deleteSelectedRules();
               }
          });
          
		JMenu subMenu = new JMenu("Add tag");
		try {
		     Collection<String> allTags = ruleset.getTags();
		     for (String tag: allTags) {
		          menuItem = new JMenuItem(tag);
		          menuItem.addActionListener(new ActionListener() {
		               public void actionPerformed(ActionEvent e) {
		                    ruleset.tagRule(listView.getSelectedValue().toString(), e.getActionCommand());
		               }
		          });
		          subMenu.add(menuItem);
		     }
		} catch (CoreException e) {
	   		 logger.warning("Could not load tags from the core.");
		}
		menuItem = new JMenuItem("New Tag...");
		menuItem.addActionListener(new ActionListener() {	
			public void actionPerformed(ActionEvent e) {
				String tag = JOptionPane.showInputDialog("Tag:");
				if (tag == null) return;
				
				ruleset.tagRule(listView.getSelectedValue().toString(), tag);
			}
		});
	    subMenu.add(menuItem);
	    popupMenu.add(subMenu);
	    
	    ArrayList<String> tags = ruleset.getRuleTags(listView.getSelectedValue().toString());
	    if (!tags.isEmpty()) {
	    	subMenu = new JMenu("Remove tag");
	    	for (String tag: tags) {
	    		menuItem = new JMenuItem(tag);
	    		menuItem.addActionListener(new ActionListener() {
	    			public void actionPerformed(ActionEvent e) {
	    				ruleset.untagRule(listView.getSelectedValue().toString(), e.getActionCommand());
	    			}
	    		});
	    		subMenu.add(menuItem);
	    	}
	    	popupMenu.add(subMenu);	
	    }
	    return popupMenu;
	}

	private ImageIcon createImageIcon(String path,
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
		enableButton = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/ComputeAdd16.gif", "Enable"));
		enableButton.setToolTipText("Enable selected rules");
		enableButton.addActionListener(new ActionListener() {

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

		disableButton = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/ComputeRemove16.gif", "Disable"));
		disableButton.setToolTipText("Disable selected rules");
		disableButton.addActionListener(new ActionListener() {

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
                         logger.log(Level.WARNING, "Could not disable selected rules");
                    }
			}
		});
		
		deleteButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/Delete16.gif", "Enable"));
          deleteButton.setToolTipText("Delete selected rules");
          deleteButton.addActionListener(new ActionListener() {

               public void actionPerformed(ActionEvent e) {
                    deleteSelectedRules();
               }
          });
          
          refreshButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/Refresh16.gif", "Refresh"));
          refreshButton.setToolTipText("Reload ruleset");
          refreshButton.addActionListener(new ActionListener() {

               public void actionPerformed(ActionEvent e) {
                    RulesBar.this.ruleset.reload();
               }
          });
          
          createRuleButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/New16.gif", "Create Rule"));
          createRuleButton.setToolTipText("Create Rule");
          createRuleButton.addActionListener(new ActionListener() {

               public void actionPerformed(ActionEvent e) {
                    String ruleName = JOptionPane.showInputDialog("Rule Name:");
                    if (ruleName == null) {
                         return;
                    }
                    try {
                         CoreGraph lhs = RulesBar.this.ruleset.getCore().createEmptyGraph();
                         CoreGraph rhs = RulesBar.this.ruleset.getCore().createEmptyGraph(); 
                         Rule<CoreGraph> rule = RulesBar.this.ruleset.getCore().createRule(ruleName, lhs, rhs);
                         
                         SplitGraphView spg = new SplitGraphView(RulesBar.this.ruleset.getCore(), rule);
                         RulesBar.this.viewPort.getViewManager().addView(spg);
                         RulesBar.this.viewPort.attachView(spg);

                         RulesBar.this.ruleset.reload();
                    } catch (CoreException ex) {
                         //We cannot create the rule. This is not critical. Inform the user.
                         logger.log(Level.WARNING, "Could not create a new rule : ", ex);
                    }
               }
          });
	}

	public RulesBar(Ruleset ruleset, ViewPort viewPort) {
		this.ruleset = ruleset;
		this.viewPort = viewPort;
		ruleset.addChangeListener(listener);

		final DefaultListCellRenderer cellRenderer = new DefaultListCellRenderer() {

            @Override
			public Component getListCellRendererComponent(
				JList list,
				Object value,
				int index,
				boolean isSelected,
				boolean cellHasFocus) {
				super.getListCellRendererComponent(list, value, 
                                        index, isSelected, cellHasFocus);
				setName(value.toString());
				if (!((RuleDescription) value).active) {
					setForeground(Color.gray);
				}
				
				return this;
			}
		};

		rulesModel = new DefaultListModel();                
		listView = new JList(rulesModel);
		listView.setCellRenderer(cellRenderer);
		listView.addMouseListener(new MouseAdapter() {
            @Override
			public void mousePressed(MouseEvent e)
			{
				if (e.isPopupTrigger()) {
                    int index = listView.locationToIndex(e.getPoint());
                    if (index < 0)
                        return;
                    if (Arrays.binarySearch(listView.getSelectedIndices(), index) < 0) {
                        listView.setSelectedIndex(index);
                    }
					JPopupMenu contextualMenu = createRuleContextualMenu();
		            contextualMenu.show(e.getComponent(),
		                       e.getX(), e.getY());
				}
			}
		});
		JScrollPane listPane = new JScrollPane(listView);
		tagsCombo = new JComboBox();
		createMenuButtons();

		
                
		tagsCombo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
                if (!suppressTagComboCallback) {
                    JComboBox cb = (JComboBox)e.getSource();
                    String tag = (String)cb.getSelectedItem();
                    loadRules(tag);
                }
			}
		});
		
		JPanel buttonBox = new JPanel();
		buttonBox.setLayout(new BoxLayout(buttonBox, BoxLayout.LINE_AXIS));
		buttonBox.add(enableButton);
		buttonBox.add(disableButton);
		buttonBox.add(deleteButton);
		buttonBox.add(createRuleButton);
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
        
	private void editRule(String rule) {

		try {
			Rule<CoreGraph> ruleGraphs = RulesBar.this.ruleset.getCore().openRule(rule);
			SplitGraphView spg = new SplitGraphView(RulesBar.this.ruleset.getCore(), ruleGraphs);
			RulesBar.this.viewPort.getViewManager().addView(spg);
			RulesBar.this.viewPort.attachView(spg);
		} catch (CoreException ex) {
			//We cannot open the rule. This is not critical. Inform the user.
			logger.log(Level.WARNING, "Could not open selected rule : ", ex);
		}
	}
     
	private void deleteSelectedRules() {
          if (JOptionPane.showConfirmDialog(null, "Delete selected rule(s)?",
                    "Delete Rules", JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION)
               return;
          try {
               Object[] descs = listView.getSelectedValues();
               for (Object d : descs) {
                    ruleset.deleteRule(((RuleDescription) d).rulename);
               }
          }
          catch (CoreException ex) {
               logger.log(Level.WARNING, "Could not disable selected rules");
          }
	}
	
	private void loadTags() {
		try {
            try {
                suppressTagComboCallback = true;
                String oldSelection = null;
                if (tagsCombo.getItemCount() > 0) {
                    oldSelection = tagsCombo.getSelectedItem().toString();
                    tagsCombo.removeAllItems();
                }
                tagsCombo.addItem("All Rules");
                for(String tag : ruleset.getTags()){
                    tagsCombo.addItem(tag);
                }
                if (oldSelection != null) {
                    for (int i = 0; i < tagsCombo.getItemCount(); ++i) {
                        if (oldSelection.equals(tagsCombo.getItemAt(i).toString())) {
                            tagsCombo.setSelectedIndex(i);
                        }
                    }
                }
            } finally {
                suppressTagComboCallback = false;
            }
		}
		catch (CoreException ex) {
			logger.log(Level.WARNING, "Could not get tags from core", ex);
		}
	}
	
	private void loadRules(String tag) {
		rulesModel.clear();
		/* If the tag exists, load the corresponding rules.
		   If not then load all the rules.*/
		try {
			if (tagsCombo.getSelectedIndex() != 0) {
				for (String rule : ruleset.getRulesByTag(tag)) {
					rulesModel.addElement(new RuleDescription(rule, 
                         ruleset.isRuleActive(rule)));
				}
			}
			else {
				for (String rule : ruleset.getRules()) {
				     rulesModel.addElement(new RuleDescription(rule, 
                         ruleset.isRuleActive(rule)));
				}
			}
		}
		catch (CoreException ex) {
			logger.log(Level.WARNING,
					   "Could not get rules and/or tags from core", ex);
		}
	}
}
