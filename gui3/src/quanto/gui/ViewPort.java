package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {
	private static final long serialVersionUID = -2789609872128334500L;
	private QuantoApp.MainMenu mainMenu = null;
	private volatile String focusedView = null;
	private JLabel pickView = null;
	private final String arrowDown = "\u25be";
	private boolean hasFocus = false;

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
				Map<String,InteractiveView> views = QuantoApp.getInstance().getViews();
				if (views.size() == 0) {
					viewMenu.add(new JMenuItem("(no views)"));
				} else {
					JMenuItem item = null;
					for (final Map.Entry<String, InteractiveView> ent : views.entrySet())
					{
						String title = ent.getKey();
						if (ent.getValue() instanceof InteractiveGraphView)
							title += " (" + ((InteractiveGraphView)ent
												.getValue()).getGraph().getName() + ")";
						item = new JMenuItem(title);
						item.setFont(item.getFont().deriveFont(12.0f));
						item.setEnabled(! ent.getValue().hasParent());
						item.addActionListener(new ActionListener() {
							public void actionPerformed(java.awt.event.ActionEvent e) {
								setFocusedView(ent.getKey());
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
		InteractiveView activeView;
		if (focusedView != null) {
			activeView = QuantoApp.getInstance().getViews().get(focusedView);
			if (activeView != null) { // this can happen if a view's name changes
				activeView.loseFocus(this);
				if (activeView != null) remove((JComponent)activeView);
			}
		}
		activeView = QuantoApp.getInstance().getViews().get(view);
		add((JComponent)activeView, BorderLayout.CENTER);
		String title = view;
		if (activeView instanceof InteractiveGraphView)
			title += " (" + ((InteractiveGraphView)activeView)
								.getGraph().getName() + ")";
		pickView.setText("  " + title + " " + arrowDown);
		focusedView = view;
		if (hasFocus) activeView.gainFocus(this);
		repaint();
	}
	
	public String getFocusedView() {
		return focusedView;
	}
	
	public void loseFocus() {
//		System.out.println("viewport losing focus");
		pickView.setForeground(Color.gray);
		hasFocus = false;
		if (focusedView != null) {
			QuantoApp.getInstance().getViews().get(focusedView).loseFocus(this);
		}
	}
	
	public void gainFocus() {
//		System.out.printf("viewport %d gaining focus\n", this.hashCode());
		pickView.setForeground(Color.black);
		hasFocus = true;
		if (focusedView != null) {
			QuantoApp.getInstance().getViews().get(focusedView).gainFocus(this);
		}
	}

	public QuantoApp.MainMenu getMainMenu() {
		return mainMenu;
	}
}
