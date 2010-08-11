/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import java.util.EventListener;

/**
 *
 * @author alex
 */
public interface TheoryListener extends EventListener {
	void ruleAdded(Theory source, String ruleName);
	void ruleDeleted(Theory source, String ruleName);
	void ruleRenamed(Theory source, String oldName, String newName);
	void rulesReloaded(Theory source);
	void activeStateChanged(Theory source, boolean active);
	void theoryRenamed(Theory source, String oldName, String newName);
	void theorySavedStateChanged(Theory source, boolean hasUnsavedChanges);
}
