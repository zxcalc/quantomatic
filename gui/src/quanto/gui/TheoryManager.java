/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.gui.QuantoCore.CoreException;

/**
 *
 * @author alex
 */
public class TheoryManager {

	private final static Logger logger =
		LoggerFactory.getLogger(TheoryManager.class);

	private QuantoCore core;
	private DefaultMutableTreeNode root;
	private DefaultTreeModel innerModel;
	private TheoryTreeModel outerModel;
	private Map<String,Theory> theoryCache = new HashMap<String, Theory>();

	public class TheoryTreeModel implements TreeModel {

		public boolean isTheoryNode(Object node) {
			if (node instanceof TheoryTreeNode)
				return true;
			else
				return false;
		}

		public boolean isRuleNode(Object node) {
			if (node instanceof TreeNode) {
				TreeNode tnode = (TreeNode)node;
				if (tnode.getParent() instanceof TheoryTreeNode)
					return true;
			}
			return false;
		}

		/**
		 * Gets the Theory associated with node.
		 *
		 * For theory nodes, this is the Theory describing the theory.
		 * For rule nodes, this is the Theory the rule is contained in.
		 * For the root node, this will be null.
		 *
		 * @param node
		 * @return
		 */
		public Theory getTheory(Object node) {
			if (node != getRoot() && node instanceof TreeNode) {
				TreeNode tnode = (TreeNode)node;
				TheoryTreeNode rsnode = null;
				if (tnode instanceof TheoryTreeNode) {
					rsnode = (TheoryTreeNode)tnode;
				} else if (tnode.getParent() instanceof TheoryTreeNode) {
					rsnode = (TheoryTreeNode)tnode.getParent();
				}
				if (rsnode != null)
					return rsnode.getTheory();
			}
			return null;
		}


		/**
		 * Gets the rule name associated with node.
		 *
		 * For rule nodes, this is the name of the rule the node represents.
		 * For theory nodes or the root node, this will be null.
		 *
		 * @param node
		 * @return
		 */
		public String getRuleName(Object node) {
			if (isRuleNode(node)) {
				return (String)((DefaultMutableTreeNode)node).getUserObject();
			}
			return null;
		}

		public Object getRoot() {
			return innerModel.getRoot();
		}

		public Object getChild(Object parent, int index) {
			return innerModel.getChild(parent, index);
		}

		public int getChildCount(Object parent) {
			return innerModel.getChildCount(parent);
		}

		public boolean isLeaf(Object node) {
			return innerModel.isLeaf(node);
		}

		public void valueForPathChanged(TreePath path, Object newValue) {
			innerModel.valueForPathChanged(path, newValue);
		}

		public int getIndexOfChild(Object parent, Object child) {
			return innerModel.getIndexOfChild(parent, child);
		}

		public void addTreeModelListener(TreeModelListener l) {
			innerModel.addTreeModelListener(l);
		}

		public void removeTreeModelListener(TreeModelListener l) {
			innerModel.removeTreeModelListener(l);
		}
	}

	private class TreeNodeLocation<T extends TreeNode> {

		public TreeNodeLocation() {
			this(null, -1);
		}

		public TreeNodeLocation(T node, int index) {
			this.node = node;
			this.index = index;
		}

		public T node;
		public int index;
	}

	private class TheoryTreeNode extends DefaultMutableTreeNode {
		public TheoryTreeNode(Theory theory) {
			super(theory);
			setAllowsChildren(true);
		}
		public Theory getTheory() {
			return (Theory)getUserObject();
		}
		public void refresh() throws QuantoCore.CoreException {
			getTheory().refreshRules();
			loadChildren();
			innerModel.nodeStructureChanged(this);
		}
		public void loadChildren() throws QuantoCore.CoreException {
			removeAllChildren();
			getTheory().loadRules();
			logger.info("Loading {} children for the theory '{}'",
				getTheory().getRules().size(),
				getTheory().getName());
			for (String rule : getTheory().getRules()) {
				DefaultMutableTreeNode ruleNode = new DefaultMutableTreeNode(rule);
				ruleNode.setAllowsChildren(false);
				add(ruleNode);
			}
		}
	}

	public TheoryManager(QuantoCore core) {
		this.core = core;
		root = new DefaultMutableTreeNode("Theories");
		root.setAllowsChildren(true);
		innerModel = new DefaultTreeModel(root);
		innerModel.setAsksAllowsChildren(true);
		outerModel = new TheoryTreeModel();
	}

	private TreeNodeLocation<TheoryTreeNode> findNode(Theory theory) {
		TheoryTreeNode rnode = null;
		int index = 0;
		Enumeration theories = root.children();
		while (theories.hasMoreElements() && rnode == null) {
			rnode = (TheoryTreeNode)theories.nextElement();
			if (rnode.getTheory() != theory) {
				rnode = null;
				++index;
			}
		}
		if (rnode == null)
			index = -1;
		return new TreeNodeLocation<TheoryTreeNode>(rnode, index);
	}

	public QuantoCore getCore() {
		return this.core;
	}
	
	public void reloadTheoriesFromCore() throws QuantoCore.CoreException {
		logger.info("Reloading theories from the backend");
		List<String> theoryNames = Arrays.asList(core.list_rulesets());
		Set<String> activeTheories = new HashSet<String>(
			Arrays.asList(core.list_active_rulesets())
			);
		logger.info("Core knows about {} theories", theoryNames.size());
		for (String name : theoryNames) {
			Theory theory = theoryCache.get(name);
			if (theory == null) {
				logger.info("Found previously unknown theory '{}'", name);
				theory = new Theory(core, name);
				theoryCache.put(name, theory);
			}
			theory.refreshRules();
			theory.setActive(activeTheories.contains(name));
		}
		root.removeAllChildren();
		loadTheoryNodes(theoryNames);
	}

	private void loadTheoryNodes(Collection<String> theories) throws QuantoCore.CoreException {
		logger.info("Loading theory nodes: {}", theories);
		for (String name : theories) {
			Theory theory = theoryCache.get(name);
			if (theory == null) {
				try {
					logger.info("Found unknown theory '{}'", name);
					theory = new Theory(core, name);
					theory.loadRules();
					theoryCache.put(name, theory);
				} catch (QuantoCore.CoreException ex) {
					logger.warn("Failed to load rules", ex);
					continue;
				}
			}
			TheoryTreeNode node = new TheoryTreeNode(theory);
			node.loadChildren();
			root.add(node);
		}
		innerModel.nodeStructureChanged(root);
	}

	public void loadTheory(String name, String fileName)
	throws QuantoCore.CoreException {
		logger.info("Loading theory {} from {}", name, fileName);
		Theory rset = core.load_ruleset(name, fileName);
		core.activate_ruleset(rset);
		theoryCache.put(rset.getName(), rset);
		TheoryTreeNode node = new TheoryTreeNode(rset);
		node.loadChildren();
		root.add(node);
		innerModel.nodesWereInserted(root, new int[] {root.getChildCount()-1});
	}

	public void unloadTheory(Theory rset)
	throws CoreException {
		logger.info("Unloading theory {}", rset.getName());
		core.unload_ruleset(rset);
		theoryCache.remove(rset.getName());
		TreeNodeLocation<TheoryTreeNode> location = findNode(rset);
		if (location.node != null) {
			root.remove(location.index);
			innerModel.nodesWereRemoved(root,
				new int[] {location.index},
				new Object[] {location.node});
		}
	}

	public String getState() {
		StringBuilder sb = new StringBuilder();
		Enumeration theories = root.children();
		while (theories.hasMoreElements()) {
			TheoryTreeNode node = (TheoryTreeNode)theories.nextElement();
			Theory rset = node.getTheory();
			sb.append(rset.getName()).append('\n');
			sb.append(rset.getPath()).append('\n');
			sb.append(rset.isActive()).append('\n');
		}
		return sb.toString();
	}

	public void loadState(String state) {
		logger.info("Loading saved state");
		String[] rsets = state.split("\\n");
		if (rsets.length % 3 != 0) {
			throw new IllegalArgumentException("state is not valid");
		}
		int idx = 0;
		String nm, path;
		boolean active;
		LinkedList<String> theories = new LinkedList<String>();

		if (root.getChildCount() > 0) {
			logger.info("Unloading existng theories");
			Enumeration oldTheories = root.children();
			while (oldTheories.hasMoreElements()) {
				TheoryTreeNode node = (TheoryTreeNode)oldTheories.nextElement();
				try {
					core.unload_ruleset(node.getTheory());
				} catch (CoreException ex) {}
			}
			root.removeAllChildren();
		}

		while (idx < rsets.length-2) {
			nm = rsets[idx];
			path = rsets[idx+1];
			active = rsets[idx+2].equals("true");

			try {
				Theory rset = core.load_ruleset(nm, path);
				if (active)
					core.activate_ruleset(rset);
				else
					core.deactivate_ruleset(rset);
				theoryCache.put(rset.getName(), rset);
				theories.addLast(nm);
			} catch (CoreException ex) {
				logger.warn("Failed to load part of state", ex);
			}

			idx+=3;
		}
		try {
			loadTheoryNodes(theories);
		} catch (CoreException ex) {
			logger.warn("Failed to load actual rules", ex);
		}
	}

	public void setTheoryActive(Theory theory, boolean active)
	throws CoreException {
		if (theory.isActive() != active) {
			logger.info("Setting '{}' theory active state to {}",
				theory.getName(), active);
			TheoryTreeNode rnode = findNode(theory).node;
			if (rnode != null) {
				logger.info("Found node, changing active state");
				if (active)
					core.activate_ruleset(theory);
				else
					core.deactivate_ruleset(theory);
				innerModel.nodeChanged(rnode);
			} else {
				logger.warn("Couldn't fine the node!");
			}
		} else {
			logger.info("'{}' theory active state is already {}",
				theory.getName(), active);
		}
	}

	public TheoryTreeModel getTreeModel() {
		return outerModel;
	}

}
