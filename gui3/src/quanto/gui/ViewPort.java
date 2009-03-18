package quanto.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {
	private static final long serialVersionUID = -2789609872128334500L;
	private final QuantoFrame quantoFrame;
	private InteractiveView activeView = null;
	private JLabel pickView = null;
	private final String arrowDown = "\u25be";

	public ViewPort(QuantoFrame quantoFrame) {
		this.quantoFrame = quantoFrame;
		setLayout(new BorderLayout());
		makeViewMenu();
		add(pickView, BorderLayout.NORTH);
	}
	
	private void makeViewMenu() {
		pickView = new JLabel("  (no views)  " + arrowDown);
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
						item = new JMenuItem(ent.getKey());
						item.setFont(item.getFont().deriveFont(12.0f));
						item.setEnabled(ent.getValue().hasParent());
						item.addActionListener(new ActionListener() {
							public void actionPerformed(java.awt.event.ActionEvent e) {
								setView(ent.getKey());
							}
						});
						viewMenu.add(item);
					}
				}
				viewMenu.show(pickView, 5, 2);
			}
		});
	}
	
	public void setView(String view) {
		if (activeView != null) remove((JComponent)activeView);
		activeView = QuantoApp.getInstance().getViews().get(view);
		if (activeView != null) add((JComponent)activeView, BorderLayout.CENTER);
		pickView.setText("  " + view + " " + arrowDown);
		repaint();
		QuantoApp.getInstance().focusView(view);
	}

	public QuantoFrame getQuantoFrame() {
		return quantoFrame;
	}
}
