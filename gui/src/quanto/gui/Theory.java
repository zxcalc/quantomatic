package quanto.gui;

import java.util.HashSet;
import java.util.Set;

import edu.uci.ics.jung.contrib.HasName;
import java.util.Arrays;
import javax.swing.event.EventListenerList;

import quanto.gui.QuantoCore.CoreException;

public class Theory implements HasName {
	private String path;
	private String name;
	private boolean active;
	private Set<String> rules = new HashSet<String>();
	private QuantoCore core;
	private boolean rulesLoaded = false;
	private boolean hasUnsavedChanges = false;
	private EventListenerList listenerList = new EventListenerList();

	public Theory(QuantoCore core, String name, String path, boolean active) {
		super();
		this.core = core;
		this.name = name;
		this.path = path;
		this.active = active;
	}
	
	public Theory(QuantoCore core, String name, String path) {
		this(core, name, path, true);
	}
	
	public Theory(QuantoCore core, String name) {
		this(core, name, "");
	}

	public boolean hasUnsavedChanges() {
		return hasUnsavedChanges;
	}

	public void markAsChanged() {
		if (!this.hasUnsavedChanges) {
			this.hasUnsavedChanges = true;
			fireTheorySavedStateChanged();
		}
	}

	public void save(String filename) throws QuantoCore.CoreException {
		core.save_ruleset(this, filename);
		path = filename;
		if (this.hasUnsavedChanges) {
			this.hasUnsavedChanges = false;
			fireTheorySavedStateChanged();
		}
	}

	public QuantoCore getCore() {
		return core;
	}

	public void addTheoryListener(TheoryListener l) {
		listenerList.add(TheoryListener.class, l);
	}

	public void removeTheoryListener(TheoryListener l) {
		listenerList.remove(TheoryListener.class, l);
	}

	private void fireRuleAdded(String ruleName) {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).ruleAdded(this, ruleName);
		 }
	     }
	}

	private void fireRuleDeleted(String ruleName) {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).ruleDeleted(this, ruleName);
		 }
	     }
	}

	private void fireRuleRenamed(String oldName, String newName) {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).ruleRenamed(this, oldName, newName);
		 }
	     }
	}

	private void fireRulesReloaded() {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).rulesReloaded(this);
		 }
	     }
	}

	private void fireActiveStateChanged() {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).activeStateChanged(this, active);
		 }
	     }
	}

	private void fireTheoryRenamed(String oldName) {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).theoryRenamed(this, oldName, name);
		 }
	     }
	}

	private void fireTheorySavedStateChanged() {
	     // Guaranteed to return a non-null array
	     Object[] listeners = listenerList.getListenerList();
	     // Process the listeners last to first, notifying
	     // those that are interested in this event
	     for (int i = listeners.length-2; i>=0; i-=2) {
		 if (listeners[i]==TheoryListener.class) {
		     ((TheoryListener)listeners[i+1]).theorySavedStateChanged(this, hasUnsavedChanges);
		 }
	     }
	}

	/**
	 * Does exactly the same as refreshRules if the rules have never been
	 * loaded.  If they have been loaded, does nothing.
	 * @throws quanto.gui.QuantoCore.CoreException
	 */
	public void loadRules() throws CoreException {
		if (!rulesLoaded)
			refreshRules();
	}
	
	public String getPath() {
		return path;
	}
	public void setPath(String path) {
		this.path = path;
	}
	public String getName() {
		return name;
	}
	/* do not call this if you are not QuantoCore, otherwise
	 * you'll be out of sync with the backend
	 */
	public void setName(String name) {
		String oldName = name;
		this.name = name;
		fireTheoryRenamed(oldName);
	}
	public boolean isActive() {
		return active;
	}
	public void setActive(boolean active) {
		if (active != this.active) {
			this.active = active;
			fireActiveStateChanged();
		}
	}
	public Set<String> getRules() {
		return rules;
	}
	@Override
	public String toString() {
		return name;
	}
	
	public void refreshRules() throws CoreException {
		String[] newRules = core.list_rules(name);
		rules.clear();
		rules.addAll(Arrays.asList(newRules));
		rulesLoaded = true;
		fireRulesReloaded();
	}

	public QuantoGraph getRuleLhs(String rule) throws CoreException {
		return core.open_rule_lhs(this, rule);
	}

	public QuantoGraph getRuleRhs(String rule) throws CoreException {
		return core.open_rule_rhs(this, rule);
	}

	public Rewrite getRule(String rule) throws CoreException {
		return new Rewrite(rule, getRuleLhs(rule), getRuleRhs(rule));
	}

	public String addRule() throws CoreException {
		QuantoGraph graph = core.new_graph();
		String ruleName = core.new_rule(this, graph);
		core.kill_graph(graph);
		if (rulesLoaded)
			rules.add(ruleName);
		fireRuleAdded(ruleName);
		markAsChanged();
		return ruleName;
	}

	public void deleteRule(String rule) throws CoreException {
		core.delete_rule(this, rule);
		if (rulesLoaded)
			rules.remove(rule);
		markAsChanged();
		fireRuleDeleted(rule);
	}

	public void renameRule(String rule, String newName) throws CoreException {
		core.rename_rule(this, rule, newName);
		if (rulesLoaded) {
			rules.remove(name);
			rules.add(newName);
		}
		markAsChanged();
		fireRuleRenamed(rule, newName);
	}
	
	public void updateRule(Rewrite rule) throws CoreException {
		core.replace_rule(this, rule);
		markAsChanged();
	}

	public void updateRule(String rule, QuantoGraph lhs, QuantoGraph rhs)
	throws CoreException {
		core.replace_rule(this, new Rewrite(rule, lhs, rhs));
		markAsChanged();
	}

	public void renameTheory(String newName) throws CoreException {
		core.rename_ruleset(this, newName);
	}
}
