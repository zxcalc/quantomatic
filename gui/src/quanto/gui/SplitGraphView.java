package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;

import quanto.gui.QuantoCore.ConsoleError;

@SuppressWarnings("serial")
public class SplitGraphView extends JPanel implements InteractiveView {
	private boolean leftFocused;
	private InteractiveGraphView leftView;
	private InteractiveGraphView rightView;
	private JSplitPane splitPane;
	private ViewPort lastViewPort; // the last viewport I saw. Could often be null.
	
	public SplitGraphView(InteractiveGraphView leftView, InteractiveGraphView rightView) {
		this(leftView, rightView, new Dimension(800,600));
	}
	
	public SplitGraphView(InteractiveGraphView leftView, InteractiveGraphView rightView, Dimension dim) {
		leftFocused = true;
		lastViewPort = null;
		this.leftView = leftView;
		this.rightView = rightView;
		
		// make some graphs to test
//		QuantoCore core = QuantoApp.getInstance().getCore();
		
		
//		try {
//			leftView = new InteractiveGraphView(core, core.new_graph(),
//					new Dimension(400, 600));
//			leftView.updateGraph();
//			rightView = new InteractiveGraphView(core, core.new_graph(),
//					new Dimension(400, 600));
//			rightView.updateGraph();
//		} catch (ConsoleError e) {
//			throw new QuantoCore.FatalError(e);
//		}
		
		leftView.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				leftFocused = true;
				updateFocus();
			}
		});
		
		rightView.addFocusListener(new FocusAdapter() {
			public void focusGained(FocusEvent e) {
				leftFocused = false;
				updateFocus();
			}
		});
		
		setLayout(new BorderLayout());
		
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(leftView);
        splitPane.setRightComponent(rightView);
        splitPane.setDividerLocation(((int)dim.getWidth() - 140) / 2);
		
		add(splitPane, BorderLayout.CENTER);
		
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
		return true;
	}

	public void viewUnfocus(ViewPort vp) {
		lastViewPort = null;
	}

	public boolean isLeftFocused() {
		return leftFocused;
	}

}
