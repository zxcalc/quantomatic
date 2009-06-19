package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

public class TheoryTree extends JPanel {
	private static final long serialVersionUID = 9201368442015685164L;
	private JTree tree;
	private DefaultMutableTreeNode top;
	protected Set<String> activeTheories;

	public TheoryTree () {
		setLayout(new BorderLayout());
		top = new DefaultMutableTreeNode("Theories");
		tree = new JTree(top);
		tree.setCellRenderer(new TheoryCellRenderer());
		activeTheories = new HashSet<String>();
		
		// don't want to steal keyboard focus from the active InteractiveView
		tree.setFocusable(false);
		setFocusable(false);
		
		add(new JScrollPane(tree), BorderLayout.CENTER);
		refresh();
	}
	
	public void refresh() {
		top.removeAllChildren();
		DefaultMutableTreeNode node;
		QuantoCore core = QuantoApp.getInstance().getCore();
		try {
			String[] thys = core.list_theories();
			updateActiveTheories(thys, core.list_active_theories());
			for (String thy : thys) {
				node = new DefaultMutableTreeNode(thy);
				for (String rule : core.list_rules(thy)) {
					node.add(new DefaultMutableTreeNode(rule));
				}
				top.add(node);
			}
		} catch (QuantoCore.ConsoleError err) {
			// should never get a console error during a refresh()
			throw new QuantoCore.FatalError(err.getMessage());
		}
		tree.expandRow(0);
	}
	
	private void updateActiveTheories(String[] all, String[] active) {
		activeTheories.clear();
		for (String s : active) activeTheories.add(s);
	}
	
	@SuppressWarnings("serial")
	private class TheoryCellRenderer extends DefaultTreeCellRenderer {

		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			// pull a nice icon'ed component from the parent
			Component cell = super.getTreeCellRendererComponent(
					tree, value, selected, expanded, leaf,row,hasFocus);
			DefaultMutableTreeNode node;
			
			try {
				// if this is a rule, check if theory is active, otherwise this
				// is the theory itself.
				if (leaf) node = (DefaultMutableTreeNode)
							((DefaultMutableTreeNode)value).getParent();
				else node = (DefaultMutableTreeNode)value;
				String thy = (String)node.getUserObject();
				
				// ghost theories that aren't active
				if (activeTheories.contains(thy) || thy=="Theories")
					cell.setForeground(Color.black);
				else
					cell.setForeground(Color.gray);
			} catch (ClassCastException e) {
				// if any of the casts fail, we've messed up in building the tree
				throw new QuantoCore.FatalError("Unexpected object in TheoryTree of type "
						+ e.getMessage());
			}
			
			return cell;
		}
	}
	
//	public static class RuleInfo {
//		public RuleInfo(String name) {
//			
//		}
//	}
}
