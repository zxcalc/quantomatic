package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {

	private static final long serialVersionUID = -2789609872128334500L;
	private QuantoApp.MainMenu mainMenu = null;
	private volatile String focusedView = null;
	private JLabel pickView = null;
	private final String arrowDown = "\u25be";
//	private boolean hasFocus = false;

	public ViewPort(QuantoApp.MainMenu mainMenu) {
		this.mainMenu = mainMenu;
		setLayout(new BorderLayout());
		makeViewMenu();
		add(pickView, BorderLayout.NORTH);
	}

	private void makeViewMenu() {
		pickView = new JLabel("  (no views)  " + arrowDown);
		pickView.setForeground(Color.gray);
		pickView.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		final JPopupMenu viewMenu = new JPopupMenu();
		pickView.addMouseListener(new MouseAdapter() {

			public void mouseClicked(MouseEvent e) {
				viewMenu.removeAll();
				Map<String, InteractiveView> views = QuantoApp.getInstance().getViewManager().getViews();
				if (views.size() == 0) {
					viewMenu.add(new JMenuItem("(no views)"));
				}
				else {
					JMenuItem item = null;
					for (final Map.Entry<String, InteractiveView> ent : views.entrySet()) {
						String title = ent.getKey();
						if (!ent.getValue().isSaved()) {
							title += "*";
						}
						if (ent.getValue() instanceof InteractiveGraphView
							&& QuantoApp.getInstance().getPreference(
							QuantoApp.SHOW_INTERNAL_NAMES)) {
							title += " (" + ((InteractiveGraphView) ent.getValue()).getGraph().getName() + ")";
						}
						item = new JMenuItem(title);
						item.setFont(item.getFont().deriveFont(12.0f));
						item.setEnabled(!ent.getValue().viewHasParent());
						item.addActionListener(new ActionListener() {

							public void actionPerformed(java.awt.event.ActionEvent e) {
								setFocusedView(ent.getKey());
								gainFocus(); // force re-focus of new view
							}
						});
						viewMenu.add(item);
					}
				}
				viewMenu.show(pickView, 5, 2);
			}
		});
	}

	public void setFocusedView(String view) {
//		System.out.println("Setting focused view to : "+view);
		InteractiveView activeView = null;
		if (focusedView != null) {
			activeView = QuantoApp.getInstance().getViewManager().getViews().get(focusedView);
			if (activeView != null) { // this can happen if a view's name changes
				activeView.viewUnfocus(this);
				if (activeView != null) {
					remove((JComponent) activeView);
				}
			}
		}
		if (view != null) {
			activeView = QuantoApp.getInstance().getViewManager().getViews().get(view);
		}
		if (activeView != null) {
			if (!(activeView instanceof JComponent)) {
				throw new QuantoCore.FatalError(
					"Attempted to focus a view that is not a JComponent!");
			}
			JComponent av = (JComponent) activeView;

			add(av, BorderLayout.CENTER);
			if (view.equals("test-split-pane")) {
				System.out.println("adding view:" + view);
			}
			focusedView = view;
			refreshLabel();
		}
		else {
			pickView.setText("  [null] " + arrowDown);
		}
		repaint();
	}

	// focus the console
	public void focusConsole() {
		setFocusedView("console");
		gainFocus();
	}

	// focus some random non-console view. Use this for when windows are closed, etc.
	public boolean focusNonConsole() {
		Map<String, InteractiveView> views = QuantoApp.getInstance().getViewManager().getViews();
		synchronized (views) {
			for (Entry<String, InteractiveView> ent : views.entrySet()) {
				if (!(ent.getValue() instanceof ConsoleView)) {
					setFocusedView(ent.getKey());
					gainFocus();
					return true;
				}
			}
		}
		focusConsole();
		gainFocus();
		return false;
	}

	public String getFocusedView() {
		return focusedView;
	}

	public void loseFocus() {
		pickView.setForeground(Color.gray);
		if (focusedView != null) {
			QuantoApp.getInstance().getViewManager().getViews().get(focusedView).viewUnfocus(this);
		}
	}

	public void gainFocus() {
		pickView.setForeground(Color.black);
		if (focusedView != null) {
			QuantoApp.getInstance().getViewManager().getViews().get(focusedView).viewFocus(this);
		}
	}

	public QuantoApp.MainMenu getMainMenu() {
		return mainMenu;
	}

	// TODO: make InteractiveGraphView call this.
	public void refreshLabel() {
		if (focusedView != null) {
			String title = focusedView;
			InteractiveView activeView = QuantoApp.getInstance().getViewManager().getViews().get(title);
			if (activeView != null) {
				if (!activeView.isSaved()) {
					title += "*";
				}
				// if the view names and graph names are out of sync, show it
				if (QuantoApp.getInstance().getPreference(QuantoApp.SHOW_INTERNAL_NAMES)) {
					if (activeView instanceof InteractiveGraphView) {
						title += " (" + ((InteractiveGraphView) activeView).getGraph().getName() + ")";
					}
					else if (activeView instanceof SplitGraphView) {
						title += String.format(" (%s -> %s)",
								       ((SplitGraphView) activeView).getLeftView().getGraph().getName(),
								       ((SplitGraphView) activeView).getRightView().getGraph().getName());
					}
				}
				pickView.setText("  " + title + " " + arrowDown);
			}
		}
	}
}
