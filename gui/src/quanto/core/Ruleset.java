/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.event.ChangeListener;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;

/**
 *
 * @author alex
 */
// FIXME: support more detailed change events
public class Ruleset {

	private final static Logger logger =
		Logger.getLogger("quanto.gui.ruleset");

	private EventListenerList listenerList = new EventListenerList();

	private Core core;
	// rule name -> active state
	private Map<String,Boolean> rules;
	// tag name -> rule names
	// assumption: we care about what rules are in a tag, rather than
	// what tags a rule has
	private Map<String,Set<String>> tags;

	public Ruleset(Core core) {
		this.core = core;
	}

	private void loadRules() throws CoreException {
		String[] allrules = core.getTalker().listRules();
		Set<String> activeRules = new HashSet<String>(Arrays.<String>asList(core.getTalker().listActiveRules()));
		rules = new TreeMap<String, Boolean>();
		for (String rule : allrules) {
			rules.put(rule, Boolean.valueOf(activeRules.contains(rule)));
		}
	}

	private void ensureRulesLoaded() throws CoreException {
		if (rules == null) {
			loadRules();
		}
	}

	private void loadTag(String tag) throws CoreException {
		ensureTagListLoaded();
		String[] tagrules = core.getTalker().listRulesByTag(tag);
		if (tagrules != null && tagrules.length > 0) {
			tags.put(tag, new HashSet<String>(Arrays.<String>asList(tagrules)));
		}
	}

	private void ensureTagLoaded(String tag) throws CoreException {
		if (tags == null || tags.get(tag) == null) {
			loadTag(tag);
		}
	}

	private void loadTagList() throws CoreException {
		tags = new TreeMap<String,Set<String>>();
		String[] allTags = core.getTalker().listTags();
		for (String tag : allTags) {
			tags.put(tag, null);
		}
	}

	private void ensureTagListLoaded() throws CoreException {
		if (tags == null) {
			loadTagList();
		}
	}

	public void reload() {
		tags = null;
		rules = null;
		fireRulesetReplaced();
	}
	
	public ArrayList<String> getRuleTags(String ruleName) throws CoreException {		
		/* Reverse lookup: obviously inefficient because of the data structure in use.*/
		ArrayList<String> ruleTags = new ArrayList<String>();
        ensureTagListLoaded();
        for (String key : tags.keySet()) {
            ensureTagLoaded(key);
            if (tags.get(key).contains(ruleName)) ruleTags.add(key);
        }
		return ruleTags;
	}

	public void tagRule(String ruleName, String tag) throws CoreException {
		core.getTalker().tagRule(ruleName, tag);
        if (tags != null) {
            boolean newTag = !tags.containsKey(tag);
            if (newTag) {
                Set<String> set = new HashSet<String>();
                set.add(ruleName);
                tags.put(tag, set);
            } else {
                tags.get(tag).add(ruleName);
            }
            fireRulesTagged(tag, Collections.singleton(ruleName), newTag);
        } else {
            // lazy
            fireRulesetReplaced();
        }
	}
	
	public void untagRule(String ruleName, String tag) throws CoreException {
        if (tags == null || tags.containsKey(tag)) {
            core.getTalker().untagRule(ruleName, tag);
            if (tags != null) {
                tags.get(tag).remove(ruleName);
                if (tags.get(tag).isEmpty()) {
                    tags.remove(tag);
                    fireRulesUntagged(tag, Collections.singleton(ruleName), true);
                } else {
                    fireRulesUntagged(tag, Collections.singleton(ruleName), false);
                }
            } else {
                // lazy
                fireRulesetReplaced();
            }
        }
	}

	public Collection<String> getTags() throws CoreException {
		ensureTagListLoaded();
		return Collections.unmodifiableCollection(tags.keySet());
	}

	public Collection<String> getRules() throws CoreException {
		ensureRulesLoaded();
		return Collections.unmodifiableCollection(rules.keySet());
	}

	public Collection<String> getRulesByTag(String tag) throws CoreException {
		ensureTagLoaded(tag);
		return Collections.unmodifiableCollection(tags.get(tag));
	}

	public boolean isRuleActive(String rule) throws CoreException {
		ensureRulesLoaded();
		return rules.get(rule);
	}

	public void activateRulesByTag(String tag) throws CoreException {
        Collection<String> changedRules = tags.get(tag);
		core.getTalker().activateRulesByTag(tag);
        Map<String,Boolean> updated = new HashMap<String, Boolean>(changedRules.size());
        for (String rule : changedRules) {
            updated.put(rule, Boolean.TRUE);
        }
        rules.putAll(updated);
        fireRulesActiveStateChanged(updated);
	}

	public void deactivateRulesByTag(String tag) throws CoreException {
        Collection<String> changedRules = tags.get(tag);
		core.getTalker().deactivateRulesByTag(tag);
        Map<String,Boolean> updated = new HashMap<String, Boolean>(changedRules.size());
        for (String rule : changedRules) {
            updated.put(rule, Boolean.FALSE);
        }
        rules.putAll(updated);
        fireRulesActiveStateChanged(updated);
	}

	public void deleteRulesByTag(String tag) throws CoreException {
        Collection<String> removedRules = tags.get(tag);
	    core.getTalker().deleteRulesByTag(tag);
        tags.remove(tag);
        rules.keySet().removeAll(removedRules);
	    fireRulesRemoved(removedRules);    
	}

	public void activateRule(String name) throws CoreException {
		core.getTalker().activateRule(name);
		rules.put(name, Boolean.TRUE);
        fireRulesActiveStateChanged(Collections.singletonMap(name, Boolean.TRUE));
	}

	public void deactivateRule(String name) throws CoreException {
		core.getTalker().deactivateRule(name);
		rules.put(name, Boolean.FALSE);
        fireRulesActiveStateChanged(Collections.singletonMap(name, Boolean.FALSE));
	}

	public void activateAllRules() throws CoreException {
        try {
            for (String name: rules.keySet()) {
                core.getTalker().activateRule(name);
                rules.put(name, Boolean.TRUE);
            }
            fireRulesActiveStateChanged(Collections.unmodifiableMap(rules));
        } catch (CoreException ex) {
            reload();
            throw ex;
        }
	}

	public void deactivateAllRules() throws CoreException {
        try {
            for (String name: rules.keySet()) {
                core.getTalker().deactivateRule(name);
                rules.put(name, Boolean.FALSE);
            }
            fireRulesActiveStateChanged(Collections.unmodifiableMap(rules));
        } catch (CoreException ex) {
            reload();
            throw ex;
        }
	}

	public void deleteRule(String rule) throws CoreException {
	     core.getTalker().deleteRule(rule);
         rules.remove(rule);
         Iterator<Set<String>> it = tags.values().iterator();
         while (it.hasNext()) {
             it.next().remove(rule);
         }
	     fireRulesRemoved(Collections.singleton(rule));
	}
	
	public void activateRules(Collection<String> ruleNames) throws CoreException {
		if (!rules.keySet().containsAll(ruleNames)) {
			throw new IllegalArgumentException("ruleNames contains unknown rules");
		}
        try {
            Map<String,Boolean> changes = new HashMap<String, Boolean>();
            for (String name: ruleNames) {
                core.getTalker().activateRule(name);
                rules.put(name, Boolean.TRUE);
                changes.put(name, Boolean.TRUE);
            }
            fireRulesActiveStateChanged(changes);
        } catch (CoreException ex) {
            reload();
            throw ex;
        }
	}

	public void deactivateRules(Collection<String> ruleNames) throws CoreException {
		if (!rules.keySet().containsAll(ruleNames)) {
			throw new IllegalArgumentException("ruleNames contains unknown rules");
		}
        try {
            Map<String,Boolean> changes = new HashMap<String, Boolean>();
            for (String name: ruleNames) {
                core.getTalker().deactivateRule(name);
                rules.put(name, Boolean.FALSE);
                changes.put(name, Boolean.FALSE);
            }
            fireRulesActiveStateChanged(changes);
        } catch (CoreException ex) {
            reload();
            throw ex;
        }
	}
    
    public void renameRule(String oldName, String newName) throws CoreException {
		if (!rules.containsKey(oldName)) {
			throw new IllegalArgumentException("Unknown rule \"" + oldName + "\"");
		}
        core.getTalker().renameRule(oldName, newName);
        Boolean active = rules.get(oldName);
        rules.remove(oldName);
        rules.put(newName, active);
        Iterator<Set<String>> it = tags.values().iterator();
        while (it.hasNext()) {
            Set<String> tagRules = it.next();
            tagRules.remove(oldName);
            tagRules.add(newName);
        }
        fireRulesRenamed(Collections.singletonMap(oldName, newName));
    }

	public void addRulesetChangeListener(RulesetChangeListener l) {
		listenerList.add(RulesetChangeListener.class, l);
	}

	public void removeRulesetChangeListener(RulesetChangeListener l) {
		listenerList.remove(RulesetChangeListener.class, l);
	}

	protected void fireRulesAdded(Collection<String> rules) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesAdded(this, rules);
            }
        }
	}

	protected void fireRulesRemoved(Collection<String> rules) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesRemoved(this, rules);
            }
        }
	}

	protected void fireRulesRenamed(Map<String,String> renaming) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesRenamed(this, renaming);
            }
        }
	}

	protected void fireRulesActiveStateChanged(Map<String,Boolean> newState) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesActiveStateChanged(this, newState);
            }
        }
	}

	protected void fireRulesTagged(String tag, Collection<String> rules, boolean created) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesTagged(this, tag, rules, created);
            }
        }
	}

	protected void fireRulesUntagged(String tag, Collection<String> rules, boolean removed) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesUntagged(this, tag, rules, removed);
            }
        }
	}

	protected void fireRulesetReplaced() {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==RulesetChangeListener.class) {
                ((RulesetChangeListener)listeners[i+1]).rulesetReplaced(this);
            }
        }
	}

    void ruleAdded(String name, boolean active) {
        rules.put(name, active);
        fireRulesAdded(Collections.singleton(name));
    }

	public Core getCore() {
		return this.core;
	}
}
