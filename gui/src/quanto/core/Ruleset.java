/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.IXMLParser;
import net.n3.nanoxml.StdXMLBuilder;
import net.n3.nanoxml.StdXMLReader;
import net.n3.nanoxml.XMLException;
import net.n3.nanoxml.XMLParserFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author alex
 */
public class Ruleset {
	private final static Logger logger =
		LoggerFactory.getLogger(CoreTalker.class);

	private Core core;
	// rule name -> active state
	private Map<String,Boolean> rules = new HashMap<String, Boolean>();
	// tag name -> rule names
	// assumption: we care about what rules are in a tag, rather than
	// what tags a rule has
	private Map<String,Set<String>> tags;

	public Ruleset(Core core) {
		this.core = core;
	}

	private void reload() throws CoreException {
		String[] allrules = core.getTalker().list_rules();
	}

	public void activateRulesByTag(String tag) throws CoreException {
		core.getTalker().activate_rules_with_tag(tag);
		Set<String> taggedRules = tags.get(tag);
		if (rules != null) {
			for (String rulename : taggedRules) {
				if (!rules.containsKey(rulename)) {
					logger.error("Inconsistent state: {} is tagged, but does not exist!", rulename);
					reload();
					return;
				}
				rules.put(rulename, Boolean.TRUE);
			}
			// Fire update!
		} else {
			logger.warn("Tag {} does not appear to exist", tag);
		}
	}

	public void deactivateRulesByTag(String tag) throws CoreException {
		core.getTalker().deactivate_rules_with_tag(tag);
		Set<String> taggedRules = tags.get(tag);
		if (rules != null) {
			for (String rulename : taggedRules) {
				if (!rules.containsKey(rulename)) {
					logger.error("Inconsistent state: {} is tagged, but does not exist!", rulename);
					reload();
					return;
				}
				rules.put(rulename, Boolean.FALSE);
			}
			// Fire update!
		} else {
			logger.warn("Tag {} does not appear to exist", tag);
		}
	}

	public void activateRule(String name) throws CoreException {
		core.getTalker().activate_rule(name);
		if (!rules.containsKey(name)) {
			logger.error("Inconsistent state: core seems to know about rule \"{}\", but we don't", name);
			reload();
			return;
		}
		rules.put(name, Boolean.TRUE);
	}

	public void deactivateRule(String name) throws CoreException {
		core.getTalker().deactivate_rule(name);
		if (!rules.containsKey(name)) {
			logger.error("Inconsistent state: core seems to know about rule \"{}\", but we don't", name);
			reload();
			return;
		}
		rules.put(name, Boolean.FALSE);
	}
}
