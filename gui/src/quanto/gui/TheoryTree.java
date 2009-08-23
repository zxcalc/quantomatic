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
	private static final List<TheoryTree> instances = new ArrayList<TheoryTree>();
	private final ViewPort viewPort;
	
	// the theory state is global
	protected static final Map<String,Theory> theories = new HashMap<String, Theory>();
	
	// the GUI components and tree model are per-instance
	private JTree tree;
	private DefaultMutableTreeNode top;

	public TheoryTree (ViewPort viewPort) {
		synchronized (instances) {
			instances.add(this);
		}
		this.viewPort = viewPort;
		setLayout(new BorderLayout());
		top = new DefaultMutableTreeNode("Theories");
		tree = new JTree(top);
		tree.setCellRenderer(new TheoryCellRenderer());
		
		// don't want to steal keyboard focus from the active InteractiveView
		tree.setFocusable(false);
		setFocusable(false);
		
		tree.addMouseListener(new MouseAdapter() {
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
							JMenuItem load = new JMenuItem("Load Theory...");
							load.addActionListener(new ActionListener() {
								public void actionPerformed(ActionEvent e) {
									QuantoApp.getInstance().loadTheory();
								}
							});
							menu.add(load);
							menu.show(tree, e.getX(), e.getY());
						} else if (o instanceof Theory) { // a theory
							//System.out.println("THEORY:" + p);
							new TheoryMenu((Theory)o).show(tree, e.getX(), e.getY());
						} else if (node.isLeaf()) { // a rule
							Theory th = (Theory)((DefaultMutableTreeNode)node.getParent()).getUserObject();
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
		synchronized (theories) {
			for (Theory thy : theories.values()) {
				node = new DefaultMutableTreeNode(thy);
				for (String rule : thy.getRules()) {
					node.add(new DefaultMutableTreeNode(rule));
				}
				top.add(node);
			}
		}
		tree.setModel(new DefaultTreeModel(top));
		
		tree.expandRow(0);
		repaint();
	}
	
	private static void updateTheories() {
		QuantoCore core = QuantoApp.getInstance().getCore();
		try {
			String[] thyNames = core.list_theories();
			String[] active = core.list_active_theories();
			Set<String> activeTheories = new HashSet<String>();
			for (String a : active) activeTheories.add(a);
			Theory thy;
			synchronized (theories) {
				for (String nm : thyNames) {
					thy = theories.get(nm);
					if (thy==null) {
						thy = new Theory(nm);
						theories.put(nm, thy);
					}
					thy.refreshRules();
					thy.setActive(activeTheories.contains(nm));
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
	public static void refreshInstances() {
		synchronized (instances) {
			updateTheories();
			for (TheoryTree t : instances) t.refresh();
			saveState();
		}
	}
	
	public static void loadTheory(String name, String fileName)
	throws QuantoCore.ConsoleError {
		Theory thy = QuantoApp.getInstance().getCore().load_theory(name, fileName);
		QuantoApp.getInstance().getCore().activate_theory(thy);
		theories.put(thy.getName(), thy);
		refreshInstances();
	}
	
	public static void unloadTheory(Theory thy) throws ConsoleError {
		QuantoApp.getInstance().getCore().unload_theory(thy);
		theories.remove(thy.getName());
		refreshInstances();
		saveState();                 
	}
	public static void saveState() {
		StringBuffer buf = new StringBuffer();
		for (Theory thy : theories.values()) {
			buf.append(thy.getName()).append("\n");
			buf.append(thy.getPath()).append("\n");
			buf.append(thy.isActive()).append("\n");
		}
		QuantoApp.getInstance().setPreference(QuantoApp.LOADED_THEORIES, buf.toString());
	}
	
	public static void loadState() {
		String[] thys = QuantoApp.getInstance()
			.getPreference(QuantoApp.LOADED_THEORIES).split("\\n");
		int idx = 0;
		String nm, path;
		boolean active;
		QuantoCore qc = QuantoApp.getInstance().getCore();
		while (idx < thys.length-2) {
			nm = thys[idx];
			path = thys[idx+1];
			active = thys[idx+2].equals("true");
//			System.out.println(active);
			try {
				Theory thy = qc.load_theory(nm, path);
				if (active) qc.activate_theory(thy);
				else qc.deactivate_theory(thy);
				theories.put(thy.getName(), thy);
			} catch (ConsoleError e) {
				System.err.printf("%s[%s,%s]\n", e.getMessage(), nm, path);
			}
			
			idx+=3;
		}
		refreshInstances();
	}
	
	
	@SuppressWarnings("serial")
	private static class TheoryCellRenderer extends DefaultTreeCellRenderer {
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			// let parent set the basic component properties
			super.getTreeCellRendererComponent(tree, value, selected, expanded,
					leaf, row, hasFocus);
			
			// ghost the theory if it isn't active
			DefaultMutableTreeNode nd = (DefaultMutableTreeNode)value;
			Theory th = null;
			if (nd.getUserObject() instanceof Theory) {
				th = (Theory)nd.getUserObject();
			} else { // we might be a rule under a theory
				nd = (DefaultMutableTreeNode)nd.getParent();
				if (nd!=null && (nd.getUserObject() instanceof Theory))
					th = (Theory)nd.getUserObject();
			}
			if (th!=null && !th.isActive()) setForeground(Color.gray);
			
			return this;
		}
	}
	
	/*
	 * this class uses the "tree" instance var
	 */
	@SuppressWarnings("serial")
	private class TheoryMenu extends JPopupMenu {
		public TheoryMenu(final Theory thy) {
			JMenuItem item;
			item = new JMenuItem("Activate");
			if (thy.isActive()) item.setEnabled(false);
			else item.addActionListener(new QuantoActionListener(tree) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					QuantoApp.getInstance().getCore().activate_theory(thy);
					TheoryTree.refreshInstances();
				}
			});
			add(item);
			item = new JMenuItem("Deactivate");
			if (!thy.isActive()) item.setEnabled(false);
			else item.addActionListener(new QuantoActionListener(tree) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					QuantoApp.getInstance().getCore().deactivate_theory(thy);
					TheoryTree.refreshInstances();
				}
			});
			add(item);
			
			item = new JMenuItem("Unload");
			item.addActionListener(new QuantoActionListener(this) {
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					TheoryTree.unloadTheory(thy);
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
		public RuleMenu(final Theory thy, final String rule) {
			JMenuItem item;
			item = new JMenuItem("Open LHS");
			
			class RuleAL extends QuantoActionListener {
				private boolean left;
				public RuleAL(boolean left) { super(tree); this.left = left; }
				
				public void wrappedAction(ActionEvent e) throws ConsoleError {
					QuantoCore core = QuantoApp.getInstance().getCore();
					QuantoGraph gr = (left) ? core.open_rule_lhs(thy, rule) : core.open_rule_rhs(thy, rule);
					InteractiveGraphView igv = new InteractiveGraphView(core, gr);
					igv.updateGraph();
					String v = QuantoApp.getInstance().addView(gr.getName(), igv);
					viewPort.setFocusedView(v);
					viewPort.gainFocus();
				}
			}
			
			item.addActionListener(new RuleAL(true));
			add(item);
			item = new JMenuItem("Open RHS");
			item.addActionListener(new RuleAL(false));
			add(item);
		}
	}
}
