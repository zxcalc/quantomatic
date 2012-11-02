/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore;

import java.util.Collection;
import java.util.EventListener;
import java.util.Map;

/**
 *
 * @author alemer
 */
public interface RulesetChangeListener extends EventListener {
    void rulesetReplaced(Ruleset source);
    void rulesAdded(Ruleset source, Collection<String> ruleNames);
    void rulesRemoved(Ruleset source, Collection<String> ruleNames);
    void rulesRenamed(Ruleset source, Map<String,String> renaming);
    void rulesActiveStateChanged(Ruleset source, Map<String,Boolean> newState);
    /**
     * Rules were tagged
     *
     * @param source the ruleset
     * @param tag the tag that was applied
     * @param ruleNames the affected rules
     * @param newTag whether this is a newly-created tag
     */
    void rulesTagged(Ruleset source, String tag, Collection<String> ruleNames, boolean newTag);
    /**
     * Rules were untagged (but not deleted)
     *
     * @param source the ruleset
     * @param tag the tag that was removed
     * @param ruleNames the affected rules
     * @param tagRemoved whether the last rule was removed (and hence the tag discarded)
     */
    void rulesUntagged(Ruleset source, String tag, Collection<String> ruleNames, boolean tagRemoved);
}
