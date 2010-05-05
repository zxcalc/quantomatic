package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import java.util.Set;
import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {

	private static final long serialVersionUID = -2789609872128334500L;
	private static Set<String> knownCommands;

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

	public static Collection<String> getKnownCommands() {
		if (knownCommands == null) {
			knownCommands = new HashSet<String>();

			knownCommands.add(UNDO_ACTION);
			knownCommands.add(REDO_ACTION);
			knownCommands.add(CUT_ACTION);
			knownCommands.add(COPY_ACTION);
			knownCommands.add(PASTE_ACTION);
			knownCommands.add(SELECT_ALL_ACTION);
			knownCommands.add(DESELECT_ALL_ACTION);

			InteractiveGraphView.registerKnownCommands();
			TextView.registerKnownCommands();
			ConsoleView.registerKnownCommands();
			SplitGraphView.registerKnownCommands();
		}
		return knownCommands;
	}
	public static void registerCommand(String command) {
		getKnownCommands().add(command);
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
		makeViewMenu();
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

	private void makeViewMenu() {
		pickView = new JLabel("  (no views)  " + arrowDown);
		pickView.setForeground(Color.gray);
		pickView.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		final JPopupMenu viewMenu = new JPopupMenu();
		pickView.addMouseListener(new MouseAdapter() {

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
							title += " (" + ((InteractiveGraphView) ent.getValue()).getGraph().getName() + ")";
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
				viewMenu.show(pickView, 5, 2);
			}
		});
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
					name += " (" + ((InteractiveGraphView) attachedView).getGraph().getName() + ")";
				}
				else if (attachedView instanceof SplitGraphView) {
					name += String.format(" (%s -> %s)",
							       ((SplitGraphView) attachedView).getLeftView().getGraph().getName(),
							       ((SplitGraphView) attachedView).getRightView().getGraph().getName());
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
