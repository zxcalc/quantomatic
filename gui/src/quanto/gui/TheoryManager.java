/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import quanto.core.TheoryListener;
import quanto.core.Theory;
import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.event.EventListenerList;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.core.CoreTalker;
import quanto.core.CoreException;

/**
 *
 * @author alex
 */
public class TheoryManager {

	private final static Logger logger =
		LoggerFactory.getLogger(TheoryManager.class);

	private CoreTalker core;
	private DefaultMutableTreeNode root;
	private DefaultTreeModel innerModel;
	private TheoryTreeModel outerModel;
	private File lastTheoryDirectory;
	private EventListenerList listenerList = new EventListenerList();
	private Map<String,Theory> theoryCache = new HashMap<String, Theory>();
	private TheoryListener listener = new TheoryListener() {
		public void ruleAdded(Theory source, String ruleName) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				node.addRule(ruleName);
				innerModel.nodesWereInserted(node,
					new int[] {node.getChildCount()-1});
			}
		}

		public void ruleDeleted(Theory source, String ruleName) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				TreeNodeLocation<DefaultMutableTreeNode> location = node.findNode(ruleName);
				if (location.index >= 0) {
					node.remove(location.index);
					innerModel.nodesWereRemoved(node,
						new int[] {location.index},
						new Object[] {location.node});
				}
			}
		}

		public void ruleRenamed(Theory source, String oldName, String newName) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				DefaultMutableTreeNode ruleNode = node.findNode(oldName).node;
				if (ruleNode != null) {
					ruleNode.setUserObject(newName);
					innerModel.nodeChanged(ruleNode);
				}
			}
		}

		public void rulesReloaded(Theory source) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				node.removeAllChildren();
				for (String rule : node.getTheory().getRules()) {
					node.addRule(rule);
				}
				innerModel.nodeStructureChanged(node);
			}
		}

		public void activeStateChanged(Theory source, boolean active) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				innerModel.nodeChanged(node);
			}
		}

		public void theoryRenamed(Theory source, String oldName, String newName) {
			TheoryTreeNode node = findNode(source).node;
			theoryCache.remove(oldName);
			theoryCache.put(source.getName(), source);
			if (node != null) {
				innerModel.nodeChanged(node);
			}
		}

		public void theorySavedStateChanged(Theory source, boolean hasUnsavedChanges) {
			TheoryTreeNode node = findNode(source).node;
			if (node != null) {
				innerModel.nodeChanged(node);
			}
			if (!hasUnsavedChanges && source.getPath() != null) {
				// it was saved, record where
				File theoryFile = new File(source.getPath());
				setLastTheoryDirectory(theoryFile.getParentFile());
			}
		}
	};

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
		private TreeNode addRule(String ruleName) {
			DefaultMutableTreeNode ruleNode = new DefaultMutableTreeNode(ruleName);
			ruleNode.setAllowsChildren(false);
			add(ruleNode);
			return ruleNode;
		}
		private TreeNodeLocation<DefaultMutableTreeNode> findNode(String rule) {
			Enumeration nodes = children();
			int index = 0;
			DefaultMutableTreeNode node = null;
			while (nodes.hasMoreElements() && node == null) {
				node = (DefaultMutableTreeNode)nodes.nextElement();
				if (!rule.equals(node.getUserObject())) {
					++index;
					node = null;
				}
			}
			if (node == null)
				index = -1;
			return new TreeNodeLocation<DefaultMutableTreeNode>(node, index);
		}
		public Theory getTheory() {
			return (Theory)userObject;
		}
		public void refresh() throws CoreException {
			getTheory().refreshRules();
			loadChildren();
			if (getParent() != null) {
				innerModel.nodeStructureChanged(this);
			}
		}
		public void loadChildren() throws CoreException {
			removeAllChildren();
			getTheory().loadRules();
			logger.debug("Loading {} children for the theory '{}'",
				getTheory().getRules().size(),
				getTheory().getName());
			for (String rule : getTheory().getRules()) {
				addRule(rule);
			}
			if (getParent() != null) {
				innerModel.nodeStructureChanged(this);
			}
		}
	}

	public TheoryManager(CoreTalker core) {
		this.core = core;
		root = new DefaultMutableTreeNode("Theories");
		root.setAllowsChildren(true);
		innerModel = new DefaultTreeModel(root);
		innerModel.setAsksAllowsChildren(true);
		outerModel = new TheoryTreeModel();
	}

	public void addRecentDirectoryChangeListener(RecentDirectoryChangeListener l)
	{
		listenerList.add(RecentDirectoryChangeListener.class, l);
	}

	public void removeRecentDirectoryChangeListener(RecentDirectoryChangeListener l)
	{
		listenerList.remove(RecentDirectoryChangeListener.class, l);
	}

	public void setLastTheoryDirectory(File directory)
	{
		if (directory.isDirectory())
		{
			lastTheoryDirectory = directory;
		        Object[] listeners = listenerList.getListenerList();
		        for (int i = listeners.length-2; i>=0; i-=2) {
				if (listeners[i]==RecentDirectoryChangeListener.class) {
					((RecentDirectoryChangeListener)listeners[i+1]).recentDirectoryChanged(this, directory);
				}
		        }
		}
	}

	public File getLastTheoryDirectory() {
		return lastTheoryDirectory;
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

	public CoreTalker getCore() {
		return this.core;
	}

	public boolean hasUnsavedTheories() {
		boolean hasUnsaved = false;
		Enumeration theories = root.children();
		while (theories.hasMoreElements()) {
			TheoryTreeNode rnode = (TheoryTreeNode)theories.nextElement();
			if (rnode.getTheory().hasUnsavedChanges()) {
				hasUnsaved = true;
				break;
			}
		}
		return hasUnsaved;
	}

	public Theory createNewTheory() throws CoreException {
		logger.debug("Creating new theory");

		Theory theory = core.new_ruleset();
		core.activate_ruleset(theory);
		theoryCache.put(theory.getName(), theory);
		TheoryTreeNode node = new TheoryTreeNode(theory);
		node.loadChildren();
		root.add(node);
		theory.addTheoryListener(listener);
		innerModel.nodesWereInserted(root, new int[] {root.getChildCount()-1});

		return theory;
	}
	
	public void reloadTheoriesFromCore() throws CoreException {
		logger.info("Reloading theories from the backend");
		List<String> theoryNames = Arrays.asList(core.list_rulesets());
		Set<String> activeTheories = new HashSet<String>(
			Arrays.asList(core.list_active_rulesets())
			);
		logger.debug("Core knows about {} theories", theoryNames.size());
		root.removeAllChildren();
		for (String name : theoryNames) {
			Theory theory = theoryCache.get(name);
			if (theory == null) {
				logger.info("Found previously unknown theory '{}'", name);
				theory = new Theory(core, name);
				theory.addTheoryListener(listener);
				theoryCache.put(name, theory);
			}
			theory.refreshRules();
			theory.setActive(activeTheories.contains(name));
		}
		loadTheoryNodes(theoryNames);
	}

	private void loadTheoryNodes(Collection<String> theories) throws CoreException {
		logger.debug("Loading theory nodes: {}", theories);
		for (String name : theories) {
			Theory theory = theoryCache.get(name);
			if (theory == null) {
				try {
					logger.info("Found unknown theory '{}'", name);
					theory = new Theory(core, name);
					theory.addTheoryListener(listener);
					theory.loadRules();
					theoryCache.put(name, theory);
				} catch (CoreException ex) {
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

	public Theory loadTheory(String name, String fileName)
	throws CoreException {
		logger.debug("Loading theory {} from {}", name, fileName);
		Theory theory = core.load_ruleset(fileName);
		core.activate_ruleset(theory);
		theoryCache.put(theory.getName(), theory);
		TheoryTreeNode node = new TheoryTreeNode(theory);
		node.loadChildren();
		root.add(node);
		theory.addTheoryListener(listener);
		innerModel.nodesWereInserted(root, new int[] {root.getChildCount()-1});

		File theoryFile = new File(fileName);
		setLastTheoryDirectory(theoryFile.getParentFile());

		return theory;
	}

	public void unloadTheory(Theory rset)
	throws CoreException {
		logger.debug("Unloading theory {}", rset.getName());
		core.unload_ruleset(rset);
		rset.removeTheoryListener(listener);
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
			Theory theory = node.getTheory();
			// only include theories that have an associated file
			if (theory.getPath() != null && theory.getPath().length() != 0) {
				sb.append(theory.getName()).append('\n');
				sb.append(theory.getPath()).append('\n');
				sb.append(theory.isActive()).append('\n');
			}
		}
		return sb.toString();
	}

	public void loadState(String state) {
		logger.debug("Loading saved state");
		String[] rsets = state.split("\\n");
		if (rsets.length % 3 != 0) {
			throw new IllegalArgumentException("state is not valid");
		}
		int idx = 0;
		String nm, path;
		boolean active;
		LinkedList<String> theories = new LinkedList<String>();

		if (root.getChildCount() > 0) {
			logger.debug("Unloading existng theories");
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
			logger.debug(active ? "Loading active theory {} from {}"
				            : "Loading inactive theory {} from {}",
				nm, path);

			try {
				Theory rset = core.load_ruleset(path);
				if (active)
					core.activate_ruleset(rset);
				else
					core.deactivate_ruleset(rset);
				rset.addTheoryListener(listener);
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
			logger.debug("Setting '{}' theory active state to {}",
				theory.getName(), active);
			TheoryTreeNode rnode = findNode(theory).node;
			if (rnode != null) {
				logger.debug("Found node, changing active state");
				if (active)
					core.activate_ruleset(theory);
				else
					core.deactivate_ruleset(theory);
				innerModel.nodeChanged(rnode);
			} else {
				logger.warn("Couldn't fine the node!");
			}
		} else {
			logger.debug("'{}' theory active state is already {}",
				theory.getName(), active);
		}
	}

	public TheoryTreeModel getTreeModel() {
		return outerModel;
	}

}
