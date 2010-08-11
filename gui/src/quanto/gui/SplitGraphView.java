package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.JSplitPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import quanto.gui.QuantoCore.CoreException;

@SuppressWarnings("serial")
public class SplitGraphView extends InteractiveView {

	public static final String USE_RULE_ACTION = "use-rule-command";

	private boolean leftFocused = true;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private volatile boolean saved;
	private Theory theory;
	// this may become null, if the rule is deleted
	private Rewrite rule;
	// we keep our own copy of this, in case someone else changes the
	// rule name in Rewrite
	private String ruleName;

	private TheoryListener listener = new TheoryListener() {

		public void ruleAdded(Theory source, String ruleName) {}
		public void activeStateChanged(Theory source, boolean active) {}
		public void theoryRenamed(Theory source, String oldName, String newName) {}

		public void rulesReloaded(Theory source) {
			// Let's just check we're still there...
			if (!source.getRules().contains(ruleName)) {
				ruleDeleted(source, ruleName);
			}
		}

		public void ruleDeleted(Theory source, String ruleName) {
			if (ruleName.equals(ruleName)) {
				rule = null;
				setSaved(false);
				if (isAttached()) {
					getViewPort().setCommandEnabled(
						InteractiveGraphView.SAVE_GRAPH_ACTION, false);
				}
			}
		}

		public void ruleRenamed(Theory source, String oldName, String newName) {
			if (rule != null && oldName.equals(ruleName)) {
				rule.setName(newName);
				setTitle(newName);
				ruleName = newName;
			}
		}

	};

	public SplitGraphView(Theory theory, String rule)
	throws QuantoCore.CoreException {
		this(theory, theory.getRule(rule));
	}

	public SplitGraphView(Theory theory, String rule, Dimension dim)
	throws QuantoCore.CoreException {
		this(theory, theory.getRule(rule), dim);
	}

	public SplitGraphView(Theory theory, Rewrite rule)
	throws QuantoCore.CoreException {
		this(theory, rule, new Dimension(800, 600));
	}

	public SplitGraphView(Theory theory, Rewrite rule, Dimension dim)
	throws QuantoCore.CoreException {
		super(rule.getName());
		this.rule = rule;
		this.ruleName = rule.getName();
		this.theory = theory;

		leftView = new InteractiveGraphView(theory.getCore(), rule.getLhs());
		leftView.setSaveEnabled(false);
		leftView.setSaveAsEnabled(false);
		leftView.updateGraph();

		rightView = new InteractiveGraphView(theory.getCore(), rule.getRhs());
		rightView.setSaveEnabled(false);
		rightView.setSaveAsEnabled(false);
		rightView.updateGraph();

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
		theory.addTheoryListener(listener);
	}

	private void setupLayout(Dimension dim) {
		setLayout(new BorderLayout());

		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(leftView);
		splitPane.setRightComponent(rightView);
		splitPane.setDividerLocation(((int) dim.getWidth() - 140) / 2);

		add(splitPane, BorderLayout.CENTER);
	}

	public boolean hasExpandingWorkspace() {
		return false;
	}

	private void updateFocus() {
		InteractiveGraphView focusMe, unfocusMe;
		if (leftFocused) {
			focusMe = leftView;
			unfocusMe = rightView;
		}
		else {
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

	public static void registerKnownCommands() {
		//ViewPort.registerCommand(USE_RULE_ACTION);
	}

	public void commandTriggered(String command) {
		if (InteractiveGraphView.SAVE_GRAPH_ACTION.equals(command)) {
			try {
				if (rule != null) {
					theory.getCore().replace_rule(theory, rule);
					setSaved(true);
				}
			}
			catch (CoreException err) {
				errorDialog(err.getMessage());
			}
		}
	}

	public void attached(ViewPort vp) {
		//vp.setCommandEnabled(USE_RULE_ACTION, true);
		vp.setCommandEnabled(InteractiveGraphView.SAVE_GRAPH_ACTION,
			rule != null && !isSaved()
			);
		updateFocus();
	}

	public void detached(ViewPort vp) {
		//vp.setCommandEnabled(USE_RULE_ACTION, false);
		vp.setCommandEnabled(InteractiveGraphView.SAVE_GRAPH_ACTION,
			false
			);
		if (leftFocused) {
			leftView.detached(vp);
		}
		else {
			rightView.detached(vp);
		}
	}

	public void cleanUp() {
		leftView.cleanUp();
		rightView.cleanUp();
		theory.removeTheoryListener(listener);
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

	public boolean isSaved() {
		return saved;
	}

	public void setSaved(boolean saved) {
		if (this.saved != saved) {
			this.saved = saved;
			if (rule != null && isAttached()) {
				getViewPort().setCommandEnabled(
					InteractiveGraphView.SAVE_GRAPH_ACTION,
					!isSaved()
					);
			}
			firePropertyChange("saved", !saved, saved);
		}
	}

	public void refresh() {
		leftView.refresh();
		rightView.refresh();
	}
}
