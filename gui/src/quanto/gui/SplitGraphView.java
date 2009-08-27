package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import quanto.gui.QuantoCore.ConsoleError;

@SuppressWarnings("serial")
public class SplitGraphView extends JPanel
implements InteractiveView {
	private boolean leftFocused;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private ViewPort lastViewPort; // the last viewport I saw. Could often be null.
	private volatile boolean saved;
	private JMenu ruleMenu;
	
	// don't trust ruleset to have much but the correct name
	private Ruleset ruleset;
	
	// rule is constructed locally
	private Rewrite rule;
	
	public SplitGraphView(Ruleset ruleset, String ruleName, 
			InteractiveGraphView leftView, InteractiveGraphView rightView) {
		this(ruleset, ruleName, leftView, rightView, new Dimension(800,600));
	}
	
	public SplitGraphView(Ruleset ruleset, String ruleName, 
			InteractiveGraphView leftView, InteractiveGraphView rightView, Dimension dim) {
		this.leftFocused = true;
		this.lastViewPort = null;
		this.leftView = leftView;
		this.rightView = rightView;
		this.ruleset = ruleset;
		this.rule = new Rewrite(ruleName,
				this.leftView.getGraph(), this.rightView.getGraph());
		
		FocusListener fl = new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				leftFocused = (e.getSource()==SplitGraphView.this.leftView);
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
        splitPane.setDividerLocation(((int)dim.getWidth() - 140) / 2);
        
        int commandMask;
	    if (QuantoApp.isMac) commandMask = Event.META_MASK;
	    else commandMask = Event.CTRL_MASK;
        
        ruleMenu = new JMenu("Rule");
        JMenuItem item = new JMenuItem("Use Rule");
        item.addActionListener(new QuantoApp.QuantoActionListener(this) {
        	public void wrappedAction(ActionEvent e) throws ConsoleError {
        		QuantoApp.getInstance().getCore().replace_rule(
        				SplitGraphView.this.ruleset,
        				SplitGraphView.this.rule);
        		// will throw error if unsuccessful, else:
        		setSaved(true);
        	}
        });
        item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_U, commandMask));
        ruleMenu.add(item);
		
		add(splitPane, BorderLayout.CENTER);
		setSaved(true);
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
		unfocusMe.setBorder(new EmptyBorder(1,1,1,1));
		if (lastViewPort != null) {
			unfocusMe.viewUnfocus(lastViewPort);
			focusMe.viewFocus(lastViewPort);
		}
	}

	public void viewFocus(ViewPort vp) {
		lastViewPort = vp;
		vp.getMainMenu().add(ruleMenu);
		updateFocus();
	}

	public boolean viewHasParent() {
		return (lastViewPort != null);
	}

	public boolean viewKill(ViewPort vp) {
		boolean kill = false;
		kill = isSaved() || (JOptionPane.showConfirmDialog(this,
				"Rule not sent to theory. Close anyway?",
				"Unsaved changes", JOptionPane.YES_NO_OPTION) == 0);
		if (kill) {
			leftView.viewKillNoPrompt();
			rightView.viewKillNoPrompt();
		}
		return kill;
	}

	public void viewUnfocus(ViewPort vp) {
		lastViewPort = null;
		vp.getMainMenu().remove(ruleMenu);
		if (leftFocused) leftView.viewUnfocus(vp);
		else rightView.viewUnfocus(vp);
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
		this.saved = saved;
	}

}
