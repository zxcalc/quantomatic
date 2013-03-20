package quanto.gui;

import com.sun.jaf.ui.ActionManager;
import com.sun.jaf.ui.UIFactory;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Image;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JSplitPane;

import javax.swing.plaf.basic.BasicSplitPaneUI;

import quanto.core.CoreException;
import quanto.gui.QuantoApp.BoolPref;

public class QuantoFrame extends JFrame implements ViewPortHost {

	private final static Logger logger = Logger.getLogger("quanto.gui");
	private static final long serialVersionUID = 3656684775223085393L;
	private final ViewPort viewPort;
	private LeftTabbedPane sidebar;
	private JSplitPane splitPane;
	private volatile static int frameCount = 0;
	private QuantoApp app;
	private ActionManager actionManager = new ActionManager();

	/**
	 * Command actions that are dealt with directly by the frame
	 * 
	 * These are "global" command (as opposed to toggle, for example) actions,
	 * such as Open or Quit.  Each should be dealt with by a separate
	 * (non-static) method in this class.
	 */
	public enum CommandAction {

		NewWindow("new-win-command", "createNewFrame"),
		NewGraph("new-graph-command", "createNewGraph"),
		OpenGraph("open-command", "openGraph"),
		LoadRuleset("load-ruleset-command", "importRuleset"),
		SaveRuleset("save-ruleset-command", "exportRuleset"),
		Close("close-command", "closeCurrentView"),
		Quit("quit-command", "quit"),
		RefreshAll("refresh-all-command", "refreshall");

		/**
		 * Create a new command action
		 * @param actionName  The action name (as in resources/actions.xml)
		 * @param methodName  The name of the method (in the QuantoFrame class)
		 *                     to invoke when the action is triggered
		 */
		private CommandAction(String actionName, String methodName) {
			this.actionName = actionName;
			this.methodName = methodName;
		}
		private final String actionName;
		private final String methodName;

		@Override
		public String toString() {
			return actionName;
		}

		public String actionName() {
			return actionName;
		}

		public String methodName() {
			return methodName;
		}
	}

	// This type has to be public in order to be registered as a
	// handler with ActionManager.  The constructor is private, however,
	// to prevent abuse.
	public class Delegate {

		private Delegate() {
		}

		public void executeCommand(String command) {
			viewPort.executeCommand(command);
		}

		public void executeCommand(String command, boolean state) {
			if (state) {
				viewPort.executeCommand(command);
			}
		}
	}
	// This type has to be public in order to be registered as a
	// handler with ActionManager.  The constructor is private, however,
	// to prevent abuse.

	public class BoolPrefDelegate {

		private final QuantoApp.BoolPref pref;

		private BoolPrefDelegate(BoolPref pref) {
			this.pref = pref;
		}

		public void setState(boolean state) {
			app.setPreference(pref, state);
		}
	}

	private void addIconFromRes(List<Image> to, String resourceRef) {
		try {
			to.add(ImageIO.read(getClass().getResource(resourceRef)));
		} catch (IOException ex) {
			logger.log(Level.WARNING, "Cannot find " + resourceRef, ex);
		}
	}

	private void loadIcons() {
		List<Image> icons = new ArrayList<Image>(6);
		addIconFromRes(icons, "/icons/quanto_icon_16.png");
		addIconFromRes(icons, "/icons/quanto_icon_24.png");
		addIconFromRes(icons, "/icons/quanto_icon_32.png");
		addIconFromRes(icons, "/icons/quanto_icon_48.png");
		addIconFromRes(icons, "/icons/quanto_icon_64.png");
		addIconFromRes(icons, "/icons/quanto_icon_128.png");
		setIconImages(icons);
	}

	public void quit() {
		app.shutdown();
	}

	public QuantoFrame(QuantoApp app) {
		super("Quantomatic");

		loadIcons();

		frameCount++;
		this.app = app;
		setBackground(Color.white);
		getContentPane().setLayout(new BorderLayout());

		actionManager.setControlConvertedToMeta(QuantoApp.isMac);
		URL actionsXml = getClass().getResource("resources/actions.xml");
		if (actionsXml == null) {
			throw new Error("Could not find resource \"resources/actions.xml\"");
		}
		try {
			actionManager.loadActions(actionsXml);
		} catch (IOException ex) {
			throw new Error("Could not load resource \"resources/actions.xml\": " + ex.getMessage());
		}
		Set<String> menuIds = actionManager.getActionListIDs();
		for (String id : actionManager.getActionIDs()) {
			if (!menuIds.contains(id)) {
				actionManager.setEnabled(id, false);
			}
		}

		for (CommandAction action : CommandAction.values()) {
			actionManager.registerCallback(action.actionName(), this, action.methodName());
			actionManager.setEnabled(action.actionName(), true);
		}
		actionManager.registerCallback("open-in-new-window-command",
				new BoolPrefDelegate(app.NEW_WINDOW_FOR_GRAPHS),
				"setState");
			actionManager.setEnabled("open-in-new-window-command", true);
			actionManager.setSelected("open-in-new-window-command",
					app.getPreference(app.NEW_WINDOW_FOR_GRAPHS));
		CommandManager commandManager = new CommandManager(actionManager);
		InteractiveGraphView.registerKnownCommands(app.getCore(), commandManager);

		UIFactory factory = new UIFactory(actionManager);
		setJMenuBar(factory.createMenuBar("main-menu"));
		if (QuantoApp.isMac) {
			removeQuitFromFileMenu();
		}
		insertTheoryMenu();
		getContentPane().add(factory.createToolBar("main-toolbar"), BorderLayout.PAGE_START);

		viewPort = new ViewPort(app.getViewManager(), this, app.getCore());
		sidebar = new LeftTabbedPane(app.getCore(), viewPort);
		commandManager.setViewPort(viewPort);

		//Add the scroll panes to a split pane.
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(sidebar);
		splitPane.setRightComponent(viewPort);
		splitPane.setDividerLocation(256);
		splitPane.setDividerSize(10);
		splitPane.setUI(new BasicSplitPaneUI());
		splitPane.setBackground(Color.DARK_GRAY);
		splitPane.setOneTouchExpandable(true);

		getContentPane().add(splitPane, BorderLayout.CENTER);
		this.pack();
	}

	/**
	 * Display an error message from the core without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  a short message explaining what could not be done
	 * @param ex  the exception thrown by the core
	 */
	public void coreErrorMessage(String message, CoreException ex) {
		// FIXME: this should be non-modal
		DetailedErrorDialog.showCoreErrorDialog(this, message, ex);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param details  a more detailed message explaining why it could not be done
	 */
	public void detailedErrorMessage(String title, String msg, String details) {
		// FIXME: this should be non-modal
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, details);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param ex  an exception detailing the error
	 */
	public void detailedErrorMessage(String title, String msg, Throwable ex) {
		// FIXME: this should be non-modal
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, ex.getLocalizedMessage());
	}

	/**
	 * Display an error message without getting in the way.
	 * 
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  the message
	 */
	public void errorMessage(String title, String msg) {
		// FIXME: this should be non-modal
		JOptionPane.showMessageDialog(this, msg, title, JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Display a modal error message from the core.
	 * 
	 * Consider whether coreErrorMessage might be less annoying.
	 *
	 * @param parent  the parent component, or null to use the frame
	 * @param msg  a short message explaining what could not be done
	 * @param ex  the exception thrown by the core
	 */
	public void coreErrorDialog(Component parent, String message, CoreException ex) {
		DetailedErrorDialog.showCoreErrorDialog(parent == null ? this : parent, message, ex);
	}

	/**
	 * Display a modal error message, with extra detail.
	 * 
	 * Consider whether detailedErrorMessage might be less annoying.
	 *
	 * @param parent  the parent component, or null to use the frame
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param details  a more detailed message explaining why it could not be done
	 */
	public void detailedErrorDialog(Component parent, String title, String msg, String details) {
		DetailedErrorDialog.showDetailedErrorDialog(parent == null ? this : parent, title, msg, details);
	}

	/**
	 * Display a modal error message, with extra detail.
	 * 
	 * Consider whether detailedErrorMessage might be less annoying.
	 *
	 * @param parent  the parent component, or null to use the frame
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param ex  an exception detailing the error
	 */
	public void detailedErrorDialog(Component parent, String title, String msg, Throwable ex) {
		DetailedErrorDialog.showDetailedErrorDialog(parent == null ? this : parent, title, msg, ex.getLocalizedMessage());
	}

	/**
	 * Display a modal error message.
	 * 
	 * Consider whether errorMessage might be less annoying.
	 *
	 * @param parent  the parent component, or null to use the frame
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 */
	public void errorDialog(Component parent, String title, String msg) {
		JOptionPane.showMessageDialog(parent == null ? this : parent, msg, title, JOptionPane.ERROR_MESSAGE);
	}

	public File openFile(String title, int type) {
		return app.openFile(this, title, type);
	}

	private void insertTheoryMenu() {
		JMenuBar menuBar = getJMenuBar();
		Action fileMenuAction = actionManager.getAction("file-menu");
		Action LoadTheoryAction = actionManager.getAction("load-theory-command");
		for (int i = 0; i < menuBar.getMenuCount(); ++i) {
			JMenu menu = menuBar.getMenu(i);
			if (menu != null && menu.getAction() == fileMenuAction) {
				for (int j = menu.getItemCount() - 1; j >= 0; --j) {
					JMenuItem item = menu.getItem(j);
					if (item != null && item.getAction() == LoadTheoryAction) {
						menu.remove(j);
						menu.add(new TheoryMenu(app.getTheoryManager(), this), j);
						return;
					}
				}
				return;
			}
		}
	}

	private void removeQuitFromFileMenu() {
		JMenuBar menuBar = getJMenuBar();
		Action fileMenuAction = actionManager.getAction("file-menu");
		Action quitCommandAction = actionManager.getAction(CommandAction.Quit.actionName());
		for (int i = 0; i < menuBar.getMenuCount(); ++i) {
			JMenu menu = menuBar.getMenu(i);
			if (menu != null && menu.getAction() == fileMenuAction) {
				for (int j = menu.getItemCount() - 1; j >= 0; --j) {
					JMenuItem item = menu.getItem(j);
					if (item != null && item.getAction() == quitCommandAction) {
						menu.remove(j);
						return;
					}
				}
				return;
			}
		}
	}

	public void refreshAll() {
		app.getViewManager().refreshAll();
	}

	public void openView(InteractiveView view) {
		if (app.getPreference(app.NEW_WINDOW_FOR_GRAPHS)) {
			app.openNewFrame(view);
		} else {
			viewPort.attachView(view);
		}
	}

	public void closeCurrentView() {
		ViewPort.CloseResult result = viewPort.closeCurrentView();
		if (result == ViewPort.CloseResult.NoMoreViews) {
			dispose();
		}
	}

	public void createNewFrame() {
		app.createNewFrame();
	}

	public void createNewGraph() {
		try {
			openView(app.createNewGraph());
		} catch (CoreException ex) {
			coreErrorDialog(this, "Could not create new graph", ex);
		}
	}

	public void importRuleset() {
		File f = app.openFile(this, "Import ruleset", QuantoApp.DIR_RULESET);
		try {
			if (f != null) {
				app.getCore().loadRuleset(f);
			}
		} catch (CoreException ex) {
			coreErrorDialog(this, "Error in core when opening \"" + f.getName() + "\"", ex);
		} catch (java.io.IOException ex) {
			detailedErrorDialog(this, "Import Ruleset", "Could not read \"" + f.getName() + "\"", ex);
		}
	}

	public void exportRuleset() {
		File f = app.saveFile(this, "Export ruleset", QuantoApp.DIR_RULESET);
		try {
			if (f != null) {
				app.getCore().saveRuleset(f);
			}
		} catch (CoreException ex) {
			coreErrorDialog(this, "Error in core when writing to \"" + f.getName() + "\"", ex);
		} catch (java.io.IOException ex) {
			detailedErrorDialog(this, "Export Ruleset", "Could not write \"" + f.getName() + "\"", ex);
		}
	}

	/**
	 * Read a graph from a file and send it to a fresh InteractiveGraphView.
	 */
	public void openGraph() {
		File f = InteractiveGraphView.chooseGraphFile(this);
		try {
			if (f != null) {
				InteractiveView view = app.openGraph(f);
				openView(view);
			}
		} catch (CoreException ex) {
			coreErrorDialog(this, "Error in core when opening \"" + f.getName() + "\"", ex);
		} catch (java.io.IOException ex) {
			detailedErrorDialog(this, "Open Graph", "Could not read \"" + f.getName() + "\"", ex);
		}
	}

	@Override
	protected void processWindowEvent(WindowEvent e) {
		if (e.getID() == WindowEvent.WINDOW_CLOSING) {
			if (frameCount == 1) {
				app.shutdown();
			} else {
				frameCount--;
				viewPort.clearPort();
				dispose();
			}
		} else {
			super.processWindowEvent(e);
		}
	}

	public ViewPort getViewPort() {
		return viewPort;
	}

	public void setViewAllowedToClose(boolean allowed) {
		actionManager.setEnabled(CommandAction.Close.actionName(), allowed);
	}

	public boolean isViewAllowedToClose() {
		return actionManager.isEnabled(CommandAction.Close.actionName());
	}

	public void setCommandEnabled(String command, boolean enabled) {
		actionManager.setEnabled(command, enabled);
	}

	public boolean isCommandEnabled(String command) {
		return actionManager.isEnabled(command);
	}

	public void setCommandStateSelected(String command, boolean selected) {
		actionManager.setSelected(command, selected);
	}

	public boolean isCommandStateSelected(String command) {
		return actionManager.isSelected(command);
	}

	public void attachedViewChanged(InteractiveView newView) {
		if (newView == null) {
			setTitle("Quantomatic");
		} else {
			setTitle("Quantomatic: " + newView.getTitle());
		}
	}
}
