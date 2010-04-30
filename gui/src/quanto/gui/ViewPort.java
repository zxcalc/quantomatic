package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.EtchedBorder;

public class ViewPort extends JPanel {

	private static final long serialVersionUID = -2789609872128334500L;
	private HashMap<String, CommandAction> actions = new HashMap<String, CommandAction>();
	private ViewPortHost host;
	private InteractiveView attachedView = null;
	private JLabel pickView = null;
	private final String arrowDown = "\u25be";
	private final InteractiveViewManager viewManager;
	private boolean showInternalNames = false;
	private ViewRenameListener viewRenameListener = new ViewRenameListener();

	public static final String UNDO_ACTION = "undo";
	public static final String REDO_ACTION = "redo";
	public static final String CUT_ACTION = "cut";
	public static final String COPY_ACTION = "copy";
	public static final String PASTE_ACTION = "paste";
	public static final String SELECT_ALL_ACTION = "select-all";
	public static final String DESELECT_ALL_ACTION = "deselect-all";

	private class ViewRenameListener implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent evt) {
			if (evt.getSource() == attachedView)
				refreshLabel();

		}
	}

	public class CommandAction extends AbstractAction {
		private List<ButtonModel> buttonModels = null;

		public CommandAction(String command) {
			putValue(ACTION_COMMAND_KEY, command);
		}

		public CommandAction(String command, String name) {
			super(name);
			putValue(ACTION_COMMAND_KEY, command);
		}

		public CommandAction(String command, String name, Icon icon) {
			super(name, icon);
			putValue(ACTION_COMMAND_KEY, command);
		}

		public String getCommand() {
			return getValue(ACTION_COMMAND_KEY).toString();
		}

		public void actionPerformed(ActionEvent e) {
			attachedView.commandTriggered(e);
			if (buttonModels != null) {
				for (ButtonModel model : buttonModels) {
					model.setSelected(true);
				}
			}
		}

		public void setSelected(boolean selected) {
			if (buttonModels != null) {
				for (ButtonModel model : buttonModels) {
					model.setSelected(selected);
				}
			}
		}

		public void disassociateButtonModel(ButtonModel model) {
			if (buttonModels != null) {
				buttonModels.remove(model);
			}
		}

		/**
		 * Associates a button model with this action, so that
		 * the selected state is set when this action is
		 * triggered.
		 *
		 * @param model
		 */
		public void associateButtonModel(ButtonModel model) {
			if (buttonModels == null) {
				buttonModels = new LinkedList<ButtonModel>();
			}
			buttonModels.add(model);
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
		createPredefinedActions();
		makeViewMenu();
		add(pickView, BorderLayout.NORTH);
	}

	private void createPredefinedActions() {
		Action undoAction = createCommandAction(UNDO_ACTION, "Undo", KeyEvent.VK_U);
		undoAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_Z, QuantoApp.COMMAND_MASK));

		Action redoAction = createCommandAction(REDO_ACTION, "Redo", KeyEvent.VK_R);
		redoAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_Y, QuantoApp.COMMAND_MASK));

		Action cutAction = createCommandAction(CUT_ACTION, "Cut", KeyEvent.VK_C);
		cutAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_X, QuantoApp.COMMAND_MASK));

		Action copyAction = createCommandAction(COPY_ACTION, "Copy", KeyEvent.VK_O);
		copyAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_C, QuantoApp.COMMAND_MASK));

		Action pasteAction = createCommandAction(PASTE_ACTION, "Paste", KeyEvent.VK_P);
		pasteAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_V, QuantoApp.COMMAND_MASK));

		Action selectAllAction = createCommandAction(
			SELECT_ALL_ACTION,
			"Select all",
			KeyEvent.VK_S);
		selectAllAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_A, QuantoApp.COMMAND_MASK));

		Action deselectAllAction = createCommandAction(
			DESELECT_ALL_ACTION,
			"Deselect all",
			KeyEvent.VK_D);
		deselectAllAction.putValue(Action.ACCELERATOR_KEY,
			KeyStroke.getKeyStroke(KeyEvent.VK_A, QuantoApp.COMMAND_MASK | KeyEvent.SHIFT_MASK));

		InteractiveGraphView.createActions(this);
		SplitGraphView.createActions(this);
		ConsoleView.createActions(this);
		TextView.createActions(this);
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
		CommandAction action = actions.get(command);
		if (action != null)
			action.setEnabled(enabled);
	}

	/**
	 * Gets a command action, creating it if it does not already exist
	 *
	 * All actions are disabled by default.
	 *
	 * @param command The command that will be passed to
	 *                InteractiveView.commandTriggered
	 * @param name The user-visible name of the action (in menus etc)
	 * @return the action associated with @p command
	 */
	public CommandAction createCommandAction(String command, String name) {
		CommandAction action = actions.get(command);
		if (action == null) {
			action = new CommandAction(command, name);
			action.setEnabled(false);
			actions.put(command, action);
		}
		return action;
	}

	/**
	 * Gets a command action, creating it if it does not already exist
	 *
	 * All actions are disabled by default.
	 *
	 * @param command The command that will be passed to
	 *                InteractiveView.commandTriggered
	 * @param name The user-visible name of the action (in menus etc)
	 * @param mnemonic The mnemonic for the action
	 * @return the action associated with @p command
	 */
	public CommandAction createCommandAction(String command, String name, int mnemonic) {
		CommandAction action = actions.get(command);
		if (action == null) {
			action = new CommandAction(command, name);
			action.putValue(Action.MNEMONIC_KEY, mnemonic);
			action.setEnabled(false);
			actions.put(command, action);
		}
		return action;
	}

	/**
	 * Gets a command action, creating it if it does not already exist
	 *
	 * All actions are disabled by default.
	 *
	 * @param command The command that will be passed to
	 *                InteractiveView.commandTriggered
	 * @param name The user-visible name of the action (in menus etc)
	 * @param icon The icon associated with the command
	 * @return the action associated with @p command
	 */
	public CommandAction createCommandAction(String command, String name, Icon icon) {
		CommandAction action = actions.get(command);
		if (action == null) {
			action = new CommandAction(command, name, icon);
			action.setEnabled(false);
			actions.put(command, action);
		}
		return action;
	}

	/**
	 * Gets a command action, creating it if it does not already exist
	 *
	 * All actions are disabled by default.
	 *
	 * @param command The command that will be passed to
	 *                InteractiveView.commandTriggered
	 * @param name The user-visible name of the action (in menus etc)
	 * @param icon The icon associated with the command
	 * @param mnemonic The mnemonic for the action
	 * @return the action associated with @p command
	 */
	public CommandAction createCommandAction(String command, String name, Icon icon, int mnemonic) {
		CommandAction action = actions.get(command);
		if (action == null) {
			action = new CommandAction(command, name, icon);
			action.putValue(Action.MNEMONIC_KEY, mnemonic);
			action.setEnabled(false);
			actions.put(command, action);
		}
		return action;
	}

	public void registerCommandAction(CommandAction action) {
		actions.put(action.getCommand(), action);
	}

	public CommandAction getCommandAction(String command) {
		return actions.get(command);
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
