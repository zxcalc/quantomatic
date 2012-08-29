package quanto.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import quanto.core.Core;

import quanto.core.CoreException;
import quanto.core.Ruleset;
import quanto.core.RulesetChangeListener;
import quanto.core.data.CoreGraph;
import quanto.core.data.Rule;

@SuppressWarnings("serial")
public class SplitGraphView extends InteractiveView {

	private boolean leftFocused = true;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private volatile boolean saved;
	// this may become null, if the rule is deleted
	private Rule<CoreGraph> rule;
	// we keep our own copy of this, in case someone else changes the
	// rule name in Rewrite
	private Core core;
	private RulesetChangeListener listener = new RulesetChangeListener() {

		public void rulesetReplaced(Ruleset source) {
			try {
				if (!core.getRuleset().getRules().contains(rule.getCoreName())) {
					if (isAttached()) {
						getViewPort().closeCurrentView();
					} else if (getViewManager() != null) {
						getViewManager().removeView(SplitGraphView.this);
					}
				}
			} catch (CoreException ex) {
				Logger.getLogger(SplitGraphView.class.getName()).log(Level.SEVERE, "Failed to get rule list.", ex);
			}
		}

		public void rulesRemoved(Ruleset source, Collection<String> ruleNames) {
			if (ruleNames.contains(rule.getCoreName())) {
				if (isAttached()) {
					getViewPort().closeCurrentView();
				} else if (getViewManager() != null) {
					getViewManager().removeView(SplitGraphView.this);
				}
			}
		}

		public void rulesRenamed(Ruleset source, Map<String, String> renaming) {
			if (renaming.containsKey(rule.getCoreName())) {
				rule.updateCoreName(renaming.get(rule.getCoreName()));
				setTitle(rule.getCoreName());
			}
		}

		public void rulesAdded(Ruleset source, Collection<String> ruleNames) {
		}

		public void rulesActiveStateChanged(Ruleset source, Map<String, Boolean> newState) {
		}

		public void rulesTagged(Ruleset source, String tag, Collection<String> ruleNames, boolean newTag) {
		}

		public void rulesUntagged(Ruleset source, String tag, Collection<String> ruleNames, boolean tagRemoved) {
		}
	};

	public SplitGraphView(Core core, Rule<CoreGraph> rule)
			throws CoreException {
		this(core, rule, new Dimension(800, 600));
	}

	public SplitGraphView(Core core, Rule<CoreGraph> rule, Dimension dim)
			throws CoreException {
		super(rule.getCoreName());
		this.rule = rule;
		this.core = core;

		core.getRuleset().addRulesetChangeListener(listener);

		leftView = new InteractiveGraphView(core, rule.getLhs());
		leftView.setSaveEnabled(false);
		leftView.setSaveAsEnabled(false);
		leftView.repaint();
		leftView.setVerticesPositionData();

		rightView = new InteractiveGraphView(core, rule.getRhs());
		rightView.setSaveEnabled(false);
		rightView.setSaveAsEnabled(false);
		rightView.repaint();
		rightView.setVerticesPositionData();
		setupListeners();
		setupLayout(dim);
		setSaved(true);
	}

	private void setupListeners() {
		FocusListener fl = new FocusAdapter() {

			@Override
			public void focusGained(FocusEvent e) {
				leftFocused = (e.getSource() == leftView);
				updateFocus();
			}
		};

		ChangeListener cl = new ChangeListener() {

			public void stateChanged(ChangeEvent e) {
				setSaved(false);
			}
		};

		leftView.addFocusListener(fl);
		rightView.addFocusListener(fl);
		leftView.addChangeListener(cl);
		rightView.addChangeListener(cl);
	}

	private void setupLayout(Dimension dim) {
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(leftView);
		splitPane.setRightComponent(rightView);
		splitPane.setDividerLocation(((int) dim.getWidth() - 140) / 2);

		setMainComponent(splitPane);
	}

	public boolean hasExpandingWorkspace() {
		return false;
	}

	private void updateFocus() {
		InteractiveGraphView focusMe, unfocusMe;
		if (leftFocused) {
			focusMe = leftView;
			unfocusMe = rightView;
		} else {
			focusMe = rightView;
			unfocusMe = leftView;
		}

		focusMe.setBorder(new LineBorder(Color.blue));
		unfocusMe.setBorder(new EmptyBorder(1, 1, 1, 1));
		if (isAttached()) {
			unfocusMe.detached(getViewPort());
			focusMe.attached(getViewPort());
		}
	}

	@Override
	public void commandTriggered(String command) {
		if (CommandManager.Command.Save.matches(command)) {
			try {
				if (rule != null) {
					core.saveRule(rule);
					setSaved(true);
				}
			} catch (CoreException err) {
				coreErrorDialog("Could not save rule", err);
			}
		} else if (CommandManager.Command.SaveAs.matches(command)) {
			try {
				String newName = JOptionPane.showInputDialog(this,
						"Rule name:",
						rule == null ? "" : rule.getCoreName());
				if (newName == null || newName.isEmpty()) {
					return;
				}

				while (core.getRuleset().getRules().contains(newName)) {
					int overwrite = JOptionPane.showConfirmDialog(this,
							"A rule named \"" + newName
							+ "\" already exists. "
							+ "Do you want to overwrite it?",
							"Overwrite rule",
							JOptionPane.YES_NO_CANCEL_OPTION);

					if (overwrite == JOptionPane.YES_OPTION) {
						break; // continue
					} else if (overwrite != JOptionPane.NO_OPTION) {
						return; // cancelled - give up
					}
					newName = JOptionPane.showInputDialog(this,
							"Rule name:",
							rule == null ? "" : rule.getCoreName());
					if (newName == null || newName.isEmpty()) {
						return;
					}
				}

				rule = core.createRule(newName, rule.getLhs(), rule.getRhs());
				setTitle(newName);
				setSaved(true);
			} catch (CoreException err) {
				coreErrorDialog("Could not save rule", err);
			}
		} else if ((CommandManager.Command.DirectedEdgeMode.matches(command))
				|| (CommandManager.Command.UndirectedEdgeMode.matches(command))
				|| (CommandManager.Command.SelectMode.matches(command))) {
			leftView.commandTriggered(command);
			rightView.commandTriggered(command);
		} else {
			if (leftFocused) {
				leftView.commandTriggered(command);
			} else {
				rightView.commandTriggered(command);
			}
		}
		super.commandTriggered(command);
	}

	@Override
	public void attached(ViewPort vp) {
		//vp.setCommandEnabled(USE_RULE_ACTION, true);
		vp.setCommandEnabled(CommandManager.Command.SaveAs, true);
		vp.setCommandEnabled(CommandManager.Command.Save,
				rule != null && !isSaved());
		updateFocus();
		super.attached(vp);
	}

	@Override
	public void detached(ViewPort vp) {
		//vp.setCommandEnabled(USE_RULE_ACTION, false);
		vp.setCommandEnabled(CommandManager.Command.SaveAs, false);
		vp.setCommandEnabled(CommandManager.Command.Save, false);
		if (leftFocused) {
			leftView.detached(vp);
		} else {
			rightView.detached(vp);
		}
		super.detached(vp);
	}

	@Override
	public void cleanUp() {
		leftView.cleanUp();
		rightView.cleanUp();
		core.getRuleset().removeRulesetChangeListener(listener);
		super.cleanUp();
	}

	@Override
	protected String getUnsavedClosingMessage() {
		return "Rule not sent to theory. Close anyway?";
	}

	public boolean isLeftFocused() {
		return leftFocused;
	}

	public InteractiveGraphView getLeftView() {
		return leftView;
	}

	public InteractiveGraphView getRightView() {
		return rightView;
	}

	@Override
	public boolean isSaved() {
		return saved;
	}

	public void setSaved(boolean saved) {
		if (this.saved != saved) {
			this.saved = saved;
			if (rule != null && isAttached()) {
				getViewPort().setCommandEnabled(
						CommandManager.Command.Save,
						!isSaved());
			}
			firePropertyChange("saved", !saved, saved);
		}
	}

	@Override
	public void refresh() {
		leftView.refresh();
		rightView.refresh();
	}
}
