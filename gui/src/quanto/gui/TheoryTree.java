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
		activeTheories = new HashSet<String>();
		tree.setCellRenderer(new TheoryCellRenderer());
		
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
			// let parent set the basic component properties
			super.getTreeCellRendererComponent(tree, value, selected, expanded,
					leaf, row, hasFocus);
			
			// ghost the theory if it isn't active
			String thy = getText();
			if (!leaf && thy!=null) {
				if (!activeTheories.contains(thy) && thy!="Theories")
					setForeground(Color.gray);
			}
			
			return this;
		}
	}
}
