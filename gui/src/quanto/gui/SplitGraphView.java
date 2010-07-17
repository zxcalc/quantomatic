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

	private boolean leftFocused;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private ViewPort lastViewPort; // the last viewport I saw. Could often be null.
	private volatile boolean saved;
	// don't trust ruleset to have much but the correct name
	private Ruleset ruleset;
	// rule is constructed locally
	private Rewrite rule;

	public SplitGraphView(Ruleset ruleset, String ruleName,
		InteractiveGraphView leftView, InteractiveGraphView rightView) {
		this(ruleset, ruleName, leftView, rightView, new Dimension(800, 600));
	}

	public SplitGraphView(Ruleset ruleset, String ruleName,
		InteractiveGraphView leftView, InteractiveGraphView rightView, Dimension dim) {
		super(ruleName);
		this.leftFocused = true;
		this.lastViewPort = null;
		this.leftView = leftView;
		this.rightView = rightView;
		this.ruleset = ruleset;
		this.rule = new Rewrite(ruleName,
			this.leftView.getGraph(), this.rightView.getGraph());

		FocusListener fl = new FocusAdapter() {

			@Override
			public void focusGained(FocusEvent e) {
				leftFocused = (e.getSource() == SplitGraphView.this.leftView);
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

		setLayout(new BorderLayout());

		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(leftView);
		splitPane.setRightComponent(rightView);
		splitPane.setDividerLocation(((int) dim.getWidth() - 140) / 2);

		add(splitPane, BorderLayout.CENTER);
		setSaved(true);
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
		if (lastViewPort != null) {
			unfocusMe.detached(lastViewPort);
			focusMe.attached(lastViewPort);
		}
	}

	public static void registerKnownCommands() {
		ViewPort.registerCommand(USE_RULE_ACTION);
	}

	public void commandTriggered(String command) {
		if (USE_RULE_ACTION.equals(command)) {
			try {
				QuantoApp.getInstance().getCore().replace_rule(
					SplitGraphView.this.ruleset,
					SplitGraphView.this.rule);
				// will throw error if unsuccessful, else:
				setSaved(true);
			}
			catch (CoreException err) {
				errorDialog(err.getMessage());
			}
		}
	}

	public void attached(ViewPort vp) {
		lastViewPort = vp;
		vp.setCommandEnabled(USE_RULE_ACTION, true);
		updateFocus();
	}

	public void detached(ViewPort vp) {
		lastViewPort = null;
		vp.setCommandEnabled(USE_RULE_ACTION, false);
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
			firePropertyChange("saved", !saved, saved);
		}
	}

	public void refresh() {
		leftView.refresh();
		rightView.refresh();
	}
}
