package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

@SuppressWarnings("serial")
public class SplitGraphView extends JPanel
implements InteractiveView {
	private boolean leftFocused;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private ViewPort lastViewPort; // the last viewport I saw. Could often be null.
	private volatile boolean saved;
	
	public SplitGraphView(InteractiveGraphView leftView, InteractiveGraphView rightView) {
		this(leftView, rightView, new Dimension(800,600));
	}
	
	public SplitGraphView(InteractiveGraphView leftView, InteractiveGraphView rightView, Dimension dim) {
		leftFocused = true;
		lastViewPort = null;
		this.leftView = leftView;
		this.rightView = rightView;
		
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
		if (leftFocused) leftView.viewUnfocus(vp);
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
