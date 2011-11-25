package quanto.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {

	private static final long serialVersionUID = -2789609872128334500L;

	private ViewPortHost host;
	private InteractiveView attachedView = null;
	private JLabel pickView = null;
	private final String arrowDown = "\u25be";
	private final InteractiveViewManager viewManager;
	private boolean showInternalNames = false;
	private ViewRenameListener viewRenameListener = new ViewRenameListener();

	public static final String UNDO_ACTION = "undo-command";
	public static final String REDO_ACTION = "redo-command";
	public static final String CUT_ACTION = "cut-command";
	public static final String COPY_ACTION = "copy-command";
	public static final String PASTE_ACTION = "paste-command";
	public static final String SELECT_ALL_ACTION = "select-all-command";
	public static final String DESELECT_ALL_ACTION = "deselect-all-command";

	private class ViewRenameListener implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent evt) {
			if (evt.getSource() == attachedView)
				refreshLabel();

		}
	}

	/**
	 * The result of closing the current view
	 */
	public enum CloseResult {
		/** The view was closed, and another view has taken its place */
		Success,
		/** The closing of the view was cancelled by the user */
		Cancelled,
		/** The view was closed, but there are no more views to replace it with */
		NoMoreViews
	}

	public ViewPort(InteractiveViewManager viewManager,
		        ViewPortHost host) {
		this.viewManager = viewManager;
		this.host = host;
		setLayout(new BorderLayout());
		pickView = makeViewMenu();
		add(pickView, BorderLayout.NORTH);
	}

	public void openView(InteractiveView view) {
		host.openView(view);
	}

	public InteractiveViewManager getViewManager() {
		return viewManager;
	}

	public void setShowInternalNames(boolean showInternalNames) {
		this.showInternalNames = showInternalNames;
	}

	public boolean showInternalNames() {
		return showInternalNames;
	}

	/**
	 * Attaches a view to this viewport, detaching any existing view.
	 * @param view The view to attach
	 * @throws ViewUnavailableException @p view is already attached to another viewport
	 */
	public void attachView(InteractiveView view)
		throws ViewUnavailableException
	{
		if (view == attachedView)
			return;
		if (view.isAttached())
			throw new ViewUnavailableException();
		detachView();
		doAttachView(view);
	}

	private void doAttachView(InteractiveView view) {
		if (attachedView != null)
			throw new IllegalStateException("There is already a view attached");
		if (view != null) {
			if (view.getParent() != null)
				throw new IllegalStateException("View '" + view.getTitle() + "' is already being displayed");

			add(view, BorderLayout.CENTER);
			attachedView = view;
			host.setViewAllowedToClose(true);
			view.setViewPort(this);
			refreshLabel();
			view.addPropertyChangeListener("title", viewRenameListener);
			view.addPropertyChangeListener("saved", viewRenameListener);
		}
		else {
			host.setViewAllowedToClose(false);
			setLabel(null);
		}
		host.attachedViewChanged(attachedView);
		validate();
		repaint();
	}

	public void detachView() {
		if (attachedView != null) {
			attachedView.removePropertyChangeListener("title", viewRenameListener);
			attachedView.removePropertyChangeListener("saved", viewRenameListener);
			attachedView.setViewPort(null);
			remove(attachedView);
			attachedView = null;
			host.setViewAllowedToClose(false);
			setLabel(null);
			host.attachedViewChanged(attachedView);
			validate();
		}
	}

	public CloseResult closeCurrentView() {
		if (attachedView == null) {
			throw new IllegalStateException("There is no currently attached view");
		}
		else {
			if (!attachedView.checkCanClose()) {
				return CloseResult.Cancelled;
			}
			InteractiveView oldView = attachedView;
			detachView();
			viewManager.removeView(oldView);
			oldView.cleanUp();

			doAttachView(viewManager.getNextFreeView());
			if (attachedView == null)
				return CloseResult.NoMoreViews;
			else
				return CloseResult.Success;
		}
	}

	public void setCommandEnabled(String command, boolean enabled) {
		host.setCommandEnabled(command, enabled);
	}
	public boolean isCommandEnabled(String command) {
		return host.isCommandEnabled(command);
	}
	public void setCommandStateSelected(String command, boolean selected) {
		host.setCommandStateSelected(command, selected);
	}
	public boolean isCommandStateSelected(String command) {
		return host.isCommandStateSelected(command);
	}

	public void executeCommand(String command) {
		if (attachedView != null)
			attachedView.commandTriggered(command);
	}

	private JLabel makeViewMenu() {
		final JLabel picker = new JLabel("  (no views)  " + arrowDown);
		picker.setOpaque(true);
		picker.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		java.net.URL imgURL = getClass().getResource("/toolbarButtonGraphics/navigation/Down16.gif");
		if (imgURL != null)
			picker.setIcon(new ImageIcon(imgURL));
		final JPopupMenu viewMenu = new JPopupMenu();
		picker.addMouseListener(new MouseAdapter() {

			@Override
			public void mouseClicked(MouseEvent e) {
				viewMenu.removeAll();
				Map<String, InteractiveView> views = viewManager.getViews();
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
							title += " (" + ((InteractiveGraphView) ent.getValue()).getGraph().getCoreName() + ")";
						}
						item = new JMenuItem(title);
						item.setFont(item.getFont().deriveFont(12.0f));
						item.setEnabled(!ent.getValue().isAttached());
						item.addActionListener(new ActionListener() {

							public void actionPerformed(java.awt.event.ActionEvent e) {
								attachView(ent.getValue());
							}
						});
						viewMenu.add(item);
					}
				}
				int yoffset = picker.getHeight();
				if (picker.getBorder() != null)
					yoffset -= picker.getBorder().getBorderInsets(picker).top;
				viewMenu.show(picker, 0, yoffset);
			}
		});
		return picker;
	}

	public InteractiveView getAttachedView() {
		return attachedView;
	}

	public void preventViewClosure() {
		host.setViewAllowedToClose(false);
	}

	private void refreshLabel() {
		if (attachedView != null) {
			String name = viewManager.getViewName(attachedView);
			if (!attachedView.isSaved()) {
				name += "*";
			}
			// if the view names and graph names are out of sync, show it
			if (showInternalNames) {
				if (attachedView instanceof InteractiveGraphView) {
					name += " (" + ((InteractiveGraphView) attachedView).getGraph().getCoreName() + ")";
				}
				else if (attachedView instanceof SplitGraphView) {
					name += String.format(" (%s -> %s)",
							       ((SplitGraphView) attachedView).getLeftView().getGraph().getCoreName(),
							       ((SplitGraphView) attachedView).getRightView().getGraph().getCoreName());
				}
			}
			setLabel(name);
		}
	}

	private void setLabel(String text) {
		if (text == null)
			text = "[null]";
		pickView.setText("  " + text + " " + arrowDown);
	}
}
