package quanto.gui;

import java.util.HashSet;
import java.util.Set;

import edu.uci.ics.jung.contrib.HasName;

import quanto.gui.QuantoCore.ConsoleError;

public class Ruleset implements HasName {
	private String path;
	private String name;
	private boolean active;
	private Set<String> rules;
	public Ruleset(String name, String path, boolean active) {
		super();
		this.name = name;
		this.path = path;
		this.active = active;
		this.rules = new HashSet<String>();
	}
	
	public Ruleset(String name, String path) {
		this(name, path, true);
	}
	
	public Ruleset(String name) {
		this(name, "");
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
	public String toString() {
		return name;
	}
	
	public void refreshRules() {
		try {
			String[] rls = QuantoApp.getInstance().getCore().list_rules(name);
			for (String r : rls) rules.add(r);
		} catch (ConsoleError e) {
			System.err.println(e.getMessage());
		}
	}
}
