/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.event.ChangeListener;

import edu.uci.ics.jung.visualization.util.ChangeEventSupport;
import edu.uci.ics.jung.visualization.util.DefaultChangeEventSupport;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author alex
 */
// FIXME: support more detailed change events
public class Ruleset implements ChangeEventSupport {

	private final static Logger logger =
		Logger.getLogger("quanto.gui.ruleset");

	private ChangeEventSupport changeSupport = new DefaultChangeEventSupport(this);

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
		fireStateChanged();
	}
	
	public ArrayList<String> getRuleTags(String ruleName) {		
		/* Reverse lookup: obviously inefficient because of the data structure in use.*/
		ArrayList<String> ruleTags = new ArrayList<String>();
		try {
			ensureTagListLoaded();
			for (String key : tags.keySet()) {
				ensureTagLoaded(key);
				if (tags.get(key).contains(ruleName)) ruleTags.add(key);
			}
			} catch (CoreException e) {
				logger.log(Level.WARNING, "Could not load tags from the core");
			}
		return ruleTags;
	}
	
	public void tagRule(String ruleName, String tag) {
		try {
			core.getTalker().tagRule(ruleName, tag);
		} catch (CoreException e) {
			logger.log(Level.WARNING, "Could not tag rule {0}", ruleName);
		}
		reload();
	}
	
	public void untagRule(String ruleName, String tag) {
		try {
			core.getTalker().untagRule(ruleName, tag);
		} catch (CoreException e) {
			logger.log(Level.WARNING, "Could not tag rule {0}", ruleName);
		}
		reload();
	}
	
	private void updateCacheByTag(String tag, Boolean newActivationState) {
		if (tags != null) {
			if (!tags.containsKey(tag)) {
				logger.log(Level.WARNING, "Inconsistent state: we don't know about tag {0}", tag);
				reload();
				return;
			}

			Set<String> taggedRules = tags.get(tag);
			if (taggedRules != null) {
				for (String rulename : taggedRules) {
					if (!rules.containsKey(rulename)) {
						logger.log(Level.WARNING, "Inconsistent state: {0} is tagged, but does not exist!", rulename);
						reload();
						return;
					}
					rules.put(rulename, Boolean.valueOf(newActivationState));
				}
			}
		}
		fireStateChanged();
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
		core.getTalker().activateRulesByTag(tag);
		updateCacheByTag(tag, Boolean.TRUE);
	}

	public void deactivateRulesByTag(String tag) throws CoreException {
		core.getTalker().deactivateRulesByTag(tag);
		updateCacheByTag(tag, Boolean.FALSE);
	}

	public void deleteRulesByTag(String tag) throws CoreException {
	     core.getTalker().deleteRulesByTag(tag);
	     reload();
	     fireStateChanged();     
	}
	
	private void updateCacheForRule(String name, Boolean newActivationState) {
		if (!rules.containsKey(name)) {
			logger.log(Level.WARNING, "Inconsistent state: core seems to know about rule \"{0}\", but we don't", name);
			reload();
			return;
		}
		rules.put(name, Boolean.valueOf(newActivationState));
		fireStateChanged();
	}

	public void activateRule(String name) throws CoreException {
		core.getTalker().activateRule(name);
		updateCacheForRule(name, Boolean.TRUE);
	}

	public void deactivateRule(String name) throws CoreException {
		core.getTalker().deactivateRule(name);
		updateCacheForRule(name, Boolean.FALSE);
	}

	public void activateAllRules() throws CoreException {
		for (String name: rules.keySet()) {
			core.getTalker().activateRule(name);
			rules.put(name, Boolean.TRUE);
		}
		fireStateChanged();
	}

	public void deactivateAllRules() throws CoreException {
		for (String name: rules.keySet()) {
			core.getTalker().deactivateRule(name);
			rules.put(name, Boolean.FALSE);
		}
		fireStateChanged();
	}

	public void deleteRule(String rule) throws CoreException {
	     core.getTalker().deleteRule(rule);
	     reload();
	     fireStateChanged();     
	}
	
	public void activateRules(Collection<String> ruleNames) throws CoreException {
		if (!rules.keySet().containsAll(ruleNames)) {
			throw new IllegalArgumentException("ruleNames contains unknown rules");
		}
		for (String name: ruleNames) {
			core.getTalker().activateRule(name);
			rules.put(name, Boolean.TRUE);
		}
		fireStateChanged();
	}

	public void deactivateRules(Collection<String> ruleNames) throws CoreException {
		if (!rules.keySet().containsAll(ruleNames)) {
			throw new IllegalArgumentException("ruleNames contains unknown rules");
		}
		for (String name: ruleNames) {
			core.getTalker().deactivateRule(name);
			rules.put(name, Boolean.FALSE);
		}
		fireStateChanged();
	}

	public void addChangeListener(ChangeListener l) {
		changeSupport.addChangeListener(l);
	}

	public void removeChangeListener(ChangeListener l) {
		changeSupport.removeChangeListener(l);
	}

	public ChangeListener[] getChangeListeners() {
		return changeSupport.getChangeListeners();
	}

	public void fireStateChanged() {
		changeSupport.fireStateChanged();
	}

	public Core getCore() {
		return this.core;
	}
}
