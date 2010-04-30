package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import quanto.gui.QuantoApp.QuantoActionListener;
import quanto.gui.QuantoCore.ConsoleError;

public class TheoryTree extends JPanel {
	private static final long serialVersionUID = 9201368442015685164L;
	// FIXME: this is only used for refreshInstances - see FIXME there
	private static final List<TheoryTree> instances = new ArrayList<TheoryTree>();
	private final ViewPort viewPort;
	private final QuantoCore core;
	
	// the theory state is global
	protected static final Map<String,Ruleset> rulesets = new HashMap<String, Ruleset>();
	
	// the GUI components and tree model are per-instance
	private JTree tree;
	private DefaultMutableTreeNode top;

	public TheoryTree (ViewPort viewPort, QuantoCore core) {
		synchronized (instances) {
			instances.add(this);
		}
		this.viewPort = viewPort;
		this.core = core;
		setLayout(new BorderLayout());
		top = new DefaultMutableTreeNode("Theories");
		tree = new JTree(top);
		tree.setCellRenderer(new TheoryCellRenderer());
		
		// don't want to steal keyboard focus from the active InteractiveView
		tree.setFocusable(false);
		setFocusable(false);
		
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				boolean rightClick = 
					(e.getButton() == MouseEvent.BUTTON3) ||
					(QuantoApp.isMac &&
					 e.isControlDown() &&
					 e.getButton() == MouseEvent.BUTTON1);
				if (rightClick) {
					TreePath p = tree.getPathForLocation(e.getX(), e.getY());
					if (p!=null) {
						DefaultMutableTreeNode node =
							(DefaultMutableTreeNode)p.getLastPathComponent();
						Object o = node.getUserObject();
						if (node.isRoot()) { // the root
							//System.out.println("ROOT:" + p);
							JPopupMenu menu = new JPopupMenu();
							JMenuItem item = new JMenuItem("Load ruleset...");
							item.addActionListener(new ActionListener() {
								public void actionPerformed(ActionEvent e) {
									QuantoApp.getInstance().loadRuleset();
								}
							});
							menu.add(item);
							
							item = new JMenuItem("Refresh rulsets");
							item.addActionListener(new ActionListener() {
								public void actionPerformed(ActionEvent e) {
									TheoryTree.refreshInstances();
								}
							});
							menu.add(item);
							
							menu.show(tree, e.getX(), e.getY());
						} else if (o instanceof Ruleset) { // a theory
							//System.out.println("THEORY:" + p);
							new RulesetMenu((Ruleset)o).show(tree, e.getX(), e.getY());
						} else if (node.isLeaf()) { // a rule
							Ruleset th = (Ruleset)((DefaultMutableTreeNode)node.getParent()).getUserObject();
							new RuleMenu(th, (String)o).show(tree, e.getX(), e.getY());
						}
					}
				}
			}
		});
		
		add(new JScrollPane(tree), BorderLayout.CENTER);
		refresh();
	}
	
	public void refresh() {
//		System.out.printf("refresh() on: %d\n", this.hashCode());
		top.removeAllChildren();
		DefaultMutableTreeNode node;
		synchronized (rulesets) {
			for (Ruleset rset : rulesets.values()) {
				node = new DefaultMutableTreeNode(rset);
				for (String rule : rset.getRules()) {
					node.add(new DefaultMutableTreeNode(rule));
				}
				top.add(node);
			}
		}
		tree.setModel(new DefaultTreeModel(top));
		
		tree.expandRow(0);
		repaint();
	}

	// FIXME: this looks like it belongs in QuantoCore
	private static void updateRulesets(QuantoCore core) {
		try {
			String[] rsetNames = core.list_rulesets();
			String[] active = core.list_active_rulesets();
			Set<String> activeTheories = new HashSet<String>();
			for (String a : active) activeTheories.add(a);
			Ruleset rset;
			synchronized (rulesets) {
				for (String nm : rsetNames) {
					rset = rulesets.get(nm);
					if (rset==null) {
						rset = new Ruleset(nm);
						rulesets.put(nm, rset);
					}
					rset.refreshRules();
					rset.setActive(activeTheories.contains(nm));
				}
			}
			
		} catch (QuantoCore.ConsoleError err) {
			// should never get a console error during an update
			throw new QuantoCore.FatalError(err.getMessage());
		}
	}
	
	/**
	 * Calls refresh() on all active instances of TheoryTree
	 */
	// FIXME: core should notify interested parties about
	//        rulesets being updated
	public static void refreshInstances() {
		QuantoCore core = QuantoApp.getInstance().getCore();
		synchronized (instances) {
			updateRulesets(core);
			for (TheoryTree t : instances) t.refresh();
			saveState();
		}
	}
	
	public static void loadRuleset(QuantoCore core, String name, String fileName)
	throws QuantoCore.ConsoleError {
		Ruleset rset = core.load_ruleset(name, fileName);
		core.activate_ruleset(rset);
		rulesets.put(rset.getName(), rset);
		refreshInstances();
	}
	
	public static void unloadRuleset(QuantoCore core, Ruleset rset) throws ConsoleError {
		core.unload_ruleset(rset);
		rulesets.remove(rset.getName());
		refreshInstances();
		saveState();
	}
	public static void saveState() {
		StringBuffer buf = new StringBuffer();
		for (Ruleset rset : rulesets.values()) {
			buf.append(rset.getName()).append("\n");
			buf.append(rset.getPath()).append("\n");
			buf.append(rset.isActive()).append("\n");
		}
		QuantoApp.getInstance().setPreference(QuantoApp.LOADED_THEORIES, buf.toString());
	}
	
	public static void loadState(QuantoCore qc, String state) {
		String[] rsets = state.split("\\n");
		int idx = 0;
		String nm, path;
		boolean active;
		while (idx < rsets.length-2) {
			nm = rsets[idx];
			path = rsets[idx+1];
			active = rsets[idx+2].equals("true");
//			System.out.println(active);
			try {
				Ruleset rset = qc.load_ruleset(nm, path);
				if (active) qc.activate_ruleset(rset);
				else qc.deactivate_ruleset(rset);
				rulesets.put(rset.getName(), rset);
			} catch (ConsoleError e) {
				System.err.printf("%s[%s,%s]\n", e.getMessage(), nm, path);
			}
			
			idx+=3;
		}
		refreshInstances();
	}
	
	
	@SuppressWarnings("serial")
	private static class TheoryCellRenderer extends DefaultTreeCellRenderer {
		@Override
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			// let parent set the basic component properties
			super.getTreeCellRendererComponent(tree, value, selected, expanded,
					leaf, row, hasFocus);
			
			// ghost the theory if it isn't active
			DefaultMutableTreeNode nd = (DefaultMutableTreeNode)value;
			Ruleset th = null;
			if (nd.getUserObject() instanceof Ruleset) {
				th = (Ruleset)nd.getUserObject();
			} else { // we might be a rule under a theory
				nd = (DefaultMutableTreeNode)nd.getParent();
				if (nd!=null && (nd.getUserObject() instanceof Ruleset))
					th = (Ruleset)nd.getUserObject();
			}
			if (th!=null && !th.isActive()) setForeground(Color.gray);
			
			return this;
		}
	}
	
	/*
	 * this class uses the "tree" instance var
	 */
	@SuppressWarnings("serial")
	private class RulesetMenu extends JPopupMenu {
		public RulesetMenu(final Ruleset rset) {
			JMenuItem item;
			item = new JMenuItem("Activate");
			if (rset.isActive()) item.setEnabled(false);
			else item.addActionListener(new QuantoActionListener(tree) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					core.activate_ruleset(rset);
					TheoryTree.refreshInstances();
				}
			});
			add(item);
			item = new JMenuItem("Deactivate");
			if (!rset.isActive()) item.setEnabled(false);
			else item.addActionListener(new QuantoActionListener(tree) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					core.deactivate_ruleset(rset);
					TheoryTree.refreshInstances();
				}
			});
			add(item);
			
			item = new JMenuItem("Unload");
			item.addActionListener(new QuantoActionListener(this) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					TheoryTree.unloadRuleset(core, rset);
				}
			});
			add(item);
		}
	}
	
	
	
	/*
	 * this class uses the "tree" instance var
	 */
	@SuppressWarnings("serial")
	private class RuleMenu extends JPopupMenu {
		public RuleMenu(final Ruleset rset, final String rule) {
			JMenuItem item;
			
			class RuleAL extends QuantoActionListener {
				private int side; // BOTH = 0, LEFT = 1, RIGHT = 2
				public RuleAL(int side) { super(tree); this.side = side; }
				
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					QuantoGraph gr1 = (side == 0 || side == 1) ? 
							core.open_rule_lhs(rset, rule) :
							core.open_rule_rhs(rset, rule);
					InteractiveGraphView igv1 = new InteractiveGraphView(core, gr1);
					igv1.updateGraph();
					
					InteractiveView view = igv1;
					if (side == 0) { // if opening both
						QuantoGraph gr2 = core.open_rule_rhs(rset, rule);
						InteractiveGraphView igv2 = new InteractiveGraphView(core, gr2);
						igv2.updateGraph();
						view = new SplitGraphView(rset, rule, igv1, igv2);
					}
					viewPort.getViewManager().addView(view);
					try {
						viewPort.attachView(view);
					}
					catch (ViewUnavailableException ex) {
						throw new Error("Caught a ViewUnavailableException: this shouldn't happen");
					}
				}
			}
			
			item = new JMenuItem("Open Rule");
			item.addActionListener(new RuleAL(0));
			add(item);
			item = new JMenuItem("Open LHS");
			item.addActionListener(new RuleAL(1));
			add(item);
			item = new JMenuItem("Open RHS");
			item.addActionListener(new RuleAL(2));
			add(item);
			
		}
	}
}
