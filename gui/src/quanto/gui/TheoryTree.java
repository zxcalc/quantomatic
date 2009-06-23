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
	
	// the theory state is global
	protected static final Map<String,Theory> theories = new HashMap<String, Theory>();
	
	// the GUI components and tree model are per-instance
	private JTree tree;
	private DefaultMutableTreeNode top;

	public TheoryTree () {
		synchronized (instances) {
			instances.add(this);
		}
		setLayout(new BorderLayout());
		top = new DefaultMutableTreeNode("Theories");
		tree = new JTree(top);
		tree.setCellRenderer(new TheoryCellRenderer());
		
		// don't want to steal keyboard focus from the active InteractiveView
		tree.setFocusable(false);
		setFocusable(false);
		
		tree.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON3) {
					TreePath p = tree.getPathForLocation(e.getX(), e.getY());
					if (p!=null) {
						DefaultMutableTreeNode node =
							(DefaultMutableTreeNode)p.getLastPathComponent();
						Object o = node.getUserObject();
						if (node.isRoot()) {
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
						} else if (o instanceof Theory) {
							//System.out.println("THEORY:" + p);
							new TheoryMenu((Theory)o).show(tree, e.getX(), e.getY());
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
	
	/*
	 * The following classes use the set activeTheories to affect their behaviour.
	 */
	
	@SuppressWarnings("serial")
	private class TheoryCellRenderer extends DefaultTreeCellRenderer {
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			// let parent set the basic component properties
			super.getTreeCellRendererComponent(tree, value, selected, expanded,
					leaf, row, hasFocus);
			
			// ghost the theory if it isn't active
			Object o = ((DefaultMutableTreeNode)value).getUserObject();
			if (o instanceof Theory) {
				if (! ((Theory)o).isActive()) setForeground(Color.gray);
			}
			
			return this;
		}
	}
	
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
		String actualName = QuantoApp.getInstance().getCore().load_theory(name, fileName);
		Theory thy = new Theory(actualName, fileName.replaceAll("\\n|\\r", ""));
		theories.put(actualName, thy);
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
		while (idx < thys.length-2) {
			nm = thys[idx];
			path = thys[idx+1];
			active = thys[idx+2].equals("true");
			try {
				String actualName = QuantoApp.getInstance().getCore().load_theory(nm, path);
				Theory thy = new Theory(actualName, path, active);
				theories.put(actualName, thy);
			} catch (ConsoleError e) {
				System.err.printf("%s[%s,%s]\n", e.getMessage(), nm, path);
			}
			
			idx+=3;
		}
		refreshInstances();
	}
}
