package quanto.gui;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

public class TheoryTree extends JPanel {
	private static final long serialVersionUID = 9201368442015685164L;
	private JTree tree;
	private DefaultMutableTreeNode top;

	public TheoryTree () {
		setLayout(new BorderLayout());
		top = new DefaultMutableTreeNode("Theories");
		tree = new JTree(top);
		
		
		// don't want to steal keyboard focus from the active InteractiveView
		tree.setFocusable(false);
		setFocusable(false);
		
		add(new JScrollPane(tree), BorderLayout.CENTER);
		refresh();
	}
	
	public void refresh() {
		top.removeAllChildren();
		DefaultMutableTreeNode thy;
		QuantoCore core = QuantoApp.getInstance().getCore();
		try {
			for (String thyName : core.list_theories()) {
				thy = new DefaultMutableTreeNode(thyName);
				for (String ruleName : core.list_rules(thyName)) {
					thy.add(new DefaultMutableTreeNode(ruleName));
				}
				top.add(thy);
			}
		} catch (QuantoCore.ConsoleError err) {
			// should never get a console error during a refresh()
			throw new QuantoCore.FatalError(err.getMessage());
		}
		tree.expandRow(0);
	}
	
//	public static class RuleInfo {
//		public RuleInfo(String name) {
//			
//		}
//	}
}
