package quanto.gui;

import java.util.HashSet;
import java.util.Set;

import edu.uci.ics.jung.contrib.HasName;
import java.util.Arrays;

import quanto.gui.QuantoCore.CoreException;

public class Theory implements HasName {
	private String path;
	private String name;
	private boolean active;
	private Set<String> rules = new HashSet<String>();
	private QuantoCore core;
	private boolean rulesLoaded = false;

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
	public void setName(String name) {
		this.name = name;
	}
	public boolean isActive() {
		return active;
	}
	public void setActive(boolean active) {
		this.active = active;
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
	}

	public QuantoGraph getRuleLhs(String rule) throws CoreException {
		return core.open_rule_lhs(this, rule);
	}

	public QuantoGraph getRuleRhs(String rule) throws CoreException {
		return core.open_rule_rhs(this, rule);
	}
}
