package quanto.gui;

import com.sun.jaf.ui.ActionManager;
import com.sun.jaf.ui.UIFactory;
import java.awt.*;
import java.awt.event.WindowEvent;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import javax.swing.*;
import quanto.gui.QuantoApp.BoolPref;

public class QuantoFrame extends JFrame implements ViewPortHost {

	private static final long serialVersionUID = 3656684775223085393L;
	private QuantoCore core;
	private final ViewPort viewPort;
	private volatile static int frameCount = 0;
	private QuantoApp app;
	private TheoryTree theoryTree;
	private ActionManager actionManager = new ActionManager();

	// these all taken from resources/actions.xml
	public final static String NEW_WINDOW_COMMAND = "new-win-command";
	public final static String NEW_GRAPH_COMMAND = "new-graph-command";
	public final static String OPEN_GRAPH_COMMAND = "open-command";
	public final static String LOAD_THEORY_COMMAND = "load-theory-command";
	public final static String CLOSE_COMMAND = "close-command";
	public final static String QUIT_COMMAND = "quit-command";
	public final static String REFRESH_ALL_COMMAND = "refresh-all-graphs-command";
	public final static String DRAW_ARROW_HEADS_COMMAND = "draw-arrow-heads-command";
	public final static String VERBOSE_CONSOLE_COMMAND = "verbose-console-command";
	public final static String SHOW_INTERNAL_GRAPH_NAMES_COMMAND = "internal-graph-names-command";
	public final static String OPEN_IN_NEW_WINDOW_COMMAND = "open-in-new-window-command";

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
			if (state)
				viewPort.executeCommand(command);
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

	public QuantoFrame(QuantoApp app) {
		super("Quantomatic");

		frameCount++;
		this.app = app;
		core = app.getCore();
		setBackground(Color.white);
		getContentPane().setLayout(new BorderLayout());

		try {
			actionManager.loadActions(QuantoFrame.class.getResource("resources/actions.xml"));
		}
		catch (IOException ex) {
			throw new Error("Could not find resource \"resources/actions.xml\": " + ex.getMessage());
		}
		Set<String> menuIds = actionManager.getActionListIDs();
		for (String id : actionManager.getActionIDs()) {
			if (!menuIds.contains(id))
				actionManager.setEnabled(id, false);
		}
		actionManager.registerCallback(NEW_WINDOW_COMMAND, app, "createNewFrame");
		actionManager.setEnabled(NEW_WINDOW_COMMAND, true);
		actionManager.registerCallback(NEW_GRAPH_COMMAND, this, "createNewGraph");
		actionManager.setEnabled(NEW_GRAPH_COMMAND, true);
		actionManager.registerCallback(OPEN_GRAPH_COMMAND, this, "openGraph");
		actionManager.setEnabled(OPEN_GRAPH_COMMAND, true);
		actionManager.registerCallback(LOAD_THEORY_COMMAND, app, "loadRuleset");
		actionManager.setEnabled(LOAD_THEORY_COMMAND, true);
		actionManager.registerCallback(CLOSE_COMMAND, this, "closeCurrentView");
		actionManager.setEnabled(CLOSE_COMMAND, true);
		actionManager.registerCallback(QUIT_COMMAND, app, "shutdown");
		actionManager.setEnabled(QUIT_COMMAND, true);
		actionManager.registerCallback(REFRESH_ALL_COMMAND, app.getViewManager(), "refreshAll");
		actionManager.setEnabled(REFRESH_ALL_COMMAND, true);
		actionManager.registerCallback(DRAW_ARROW_HEADS_COMMAND,
			new BoolPrefDelegate(QuantoApp.DRAW_ARROW_HEADS),
			"setState");
		actionManager.setEnabled(DRAW_ARROW_HEADS_COMMAND, true);
		actionManager.registerCallback(VERBOSE_CONSOLE_COMMAND,
			new BoolPrefDelegate(QuantoApp.CONSOLE_ECHO),
			"setState");
		actionManager.setEnabled(VERBOSE_CONSOLE_COMMAND, true);
		actionManager.registerCallback(SHOW_INTERNAL_GRAPH_NAMES_COMMAND,
			new BoolPrefDelegate(QuantoApp.SHOW_INTERNAL_NAMES),
			"setState");
		actionManager.setEnabled(SHOW_INTERNAL_GRAPH_NAMES_COMMAND, true);
		actionManager.registerCallback(OPEN_IN_NEW_WINDOW_COMMAND,
			new BoolPrefDelegate(QuantoApp.NEW_WINDOW_FOR_GRAPHS),
			"setState");
		actionManager.setEnabled(OPEN_IN_NEW_WINDOW_COMMAND, true);

		UIFactory factory = new UIFactory(actionManager);
		setJMenuBar(factory.createMenuBar("main-menu"));
		getContentPane().add(factory.createToolBar("main-toolbar"), BorderLayout.PAGE_START);

		viewPort = new ViewPort(app.getViewManager(), this);
		theoryTree = new TheoryTree(viewPort, core);

		Delegate delegate = new Delegate();
		actionManager.registerGenericCallback(
			ViewPort.getKnownCommands(),
			delegate, "executeCommand");

		//Add the scroll panes to a split pane.
		JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(theoryTree);
		splitPane.setRightComponent(viewPort);
		splitPane.setDividerLocation(150);

		getContentPane().add(splitPane, BorderLayout.CENTER);

		this.pack();
	}

	public void openView(InteractiveView view) {
		if (app.getPreference(QuantoApp.NEW_WINDOW_FOR_GRAPHS)) {
			app.openNewFrame(view);
		}
		else {
			viewPort.attachView(view);
		}
	}

	public void closeCurrentView() {
		ViewPort.CloseResult result = viewPort.closeCurrentView();
		if (result == ViewPort.CloseResult.NoMoreViews)
			dispose();
	}

	public void createNewGraph() {
		try {
			openView(app.createNewGraph());
		}
		catch (QuantoCore.ConsoleError ex) {
			app.errorDialog("Could not create new graph: " + ex.getMessage());
		}
	}

	/**
	 * Read a graph from a file and send it to a fresh InteractiveGraphView.
	 */
	public void openGraph() {
		String lastDir = app.getPreference(QuantoApp.LAST_OPEN_DIR);

		JFileChooser fc = app.getFileChooser();

		if (lastDir != null) {
			fc.setCurrentDirectory(new File(lastDir));
		}

		int retVal = fc.showDialog(null, "Open");
		if (retVal == JFileChooser.APPROVE_OPTION) {
			File f = fc.getSelectedFile();
			try {
				if (f.getParent() != null) {
					app.setPreference(QuantoApp.LAST_OPEN_DIR, f.getParent());
				}
				InteractiveView view = app.openGraph(f);

				openView(view);
			}
			catch (QuantoCore.ConsoleError e) {
				app.errorDialog("Error in core when opening \"" + f.getName() + "\": " + e.getMessage());
			}
			catch (QuantoGraph.ParseException e) {
				app.errorDialog("\"" + f.getName() + "\" is in the wrong format or corrupted: " + e.getMessage());
			}
			catch (java.io.IOException e) {
				app.errorDialog("Could not read \"" + f.getName() + "\": " + e.getMessage());
			}
		}
	}

	@Override
	protected void processWindowEvent(WindowEvent e) {
		if (e.getID() == WindowEvent.WINDOW_CLOSING) {
			if (frameCount == 1) {
				app.shutdown();
			}
			else {
				frameCount--;
				viewPort.detachView();
				dispose();
			}
		}
		else {
			super.processWindowEvent(e);
		}
	}

	public QuantoCore getCore() {
		return core;
	}

	public ViewPort getViewPort() {
		return viewPort;
	}

	public void setViewAllowedToClose(boolean allowed) {
		actionManager.setEnabled(CLOSE_COMMAND, allowed);
	}

	public boolean isViewAllowedToClose() {
		return actionManager.isEnabled(CLOSE_COMMAND);
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
}
