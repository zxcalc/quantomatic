// vim:sts=8:ts=8:noet:sw=8
package quanto.gui;

import quanto.core.QuantoGraph;
import quanto.core.CoreTalker;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.util.prefs.Preferences;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import apple.dts.samplecode.osxadapter.OSXAdapter;
import java.io.FileWriter;
import java.io.IOException;
import quanto.core.Core;
import quanto.core.CoreConsoleTalker;
import quanto.core.CoreException;
/**
 * Singleton class 
 * @author aleks
 *
 */
public class QuantoApp {

	private final static Logger logger =
		LoggerFactory.getLogger(QuantoApp.class);

	// isMac is used for CTRL vs META shortcuts, etc
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	// MAC_OS_X is used to determine whether we use OSXAdapter to
	// hook into the application menu
	public static final boolean MAC_OS_X =
		(System.getProperty("os.name").toLowerCase().startsWith("mac os x"));
	public static final int COMMAND_MASK =
		MAC_OS_X ? java.awt.event.InputEvent.META_DOWN_MASK
		         : java.awt.event.InputEvent.CTRL_DOWN_MASK;
	private static QuantoApp theApp = null;
	public static boolean useExperimentalLayout = false;

	private final Preferences globalPrefs;
	private final Core core;
	private JFileChooser fileChooser = null;
	private final InteractiveViewManager viewManager;

	private static class Pref<T> {

		final T def; // default value
		final String key;
		String friendlyName;

		protected Pref(String key, T def) {
			this.key = key;
			this.def = def;
		}

		protected Pref(String key, T def, String friendlyName) {
			this.key = key;
			this.def = def;
			this.friendlyName = friendlyName;
		}

		public String getFriendlyName() {
			return friendlyName;
		}
	}

	public static class StringPref extends Pref<String> {

		protected StringPref(String key, String def) {
			super(key, def);
		}
	}

	public static class BoolPref extends Pref<Boolean> implements ItemListener {

		protected BoolPref(String key, Boolean def) {
			super(key, def);
		}

		protected BoolPref(String key, Boolean def, String friendlyName) {
			super(key, def, friendlyName);
		}

		public void itemStateChanged(ItemEvent e) {
			QuantoApp.getInstance().setPreference(this, e.getStateChange() == ItemEvent.SELECTED);
		}
	}
	// Preferences
	public static final BoolPref DRAW_ARROW_HEADS =
		new BoolPref("draw_arrow_heads", false, "Draw arrow geads");
	public static final BoolPref NEW_WINDOW_FOR_GRAPHS =
		new BoolPref("new_window_for_graphs", false, "Open graphs in a new window");
	public static final BoolPref SHOW_INTERNAL_NAMES =
		new BoolPref("show_internal_names", false, "Show internal graph names");
	public static final StringPref LAST_OPEN_DIR =
		new StringPref("last_open_dir", null);
	public static final StringPref LAST_THEORY_OPEN_DIR =
		new StringPref("last_theory_open_dir", null);
	public static final StringPref SAVED_RULESET =
		new StringPref("saved_ruleset", "");

	public static QuantoApp getInstance() {
		if (theApp == null) {
			try {
				theApp = new QuantoApp();
			} catch (CoreException ex) {
				// FATAL!!!
				logger.error("Failed to start core: terminating", ex);
				JOptionPane.showMessageDialog(null,
					ex.getMessage(),
					"Could not start core",
					JOptionPane.ERROR_MESSAGE);
				System.exit(1);
			}
		}
		return theApp;
	}

	public static boolean hasInstance() {
		return !(theApp == null);
	}

	/**
	 * main entry point for the GUI application
	 * @param args
	 */
	public static void main(String[] args) {
		logger.info("Starting quantomatic");
		boolean mathematicaMode = false;
		for (String arg : args) {
			if (arg.equals("--app-mode")) {
				String appName = "Quantomatic.app";

				// determine the app name from the classpath if I can...
				String classpath = System.getProperty("java.class.path");
				logger.debug("Trying to determine app name using class path ({})", classpath);
				for (String path : classpath.split(System.getProperty("path.separator"))) {
					if (path.indexOf("QuantoGui.jar") != -1) {
						String[] dirs = path.split(System.getProperty("file.separator"));
						if (dirs.length >= 5) {
							appName = dirs[dirs.length - 5];
						}
					}
				}

				logger.info("Invoked as OS X application ({})", appName);
				edu.uci.ics.jung.contrib.DotLayout.dotProgram =
					appName + "/Contents/MacOS/dot_static";
				CoreTalker.quantoCoreExecutable =
					appName + "/Contents/MacOS/quanto-core-app";
			}
			else if (arg.equals("--mathematica-mode")) {
				mathematicaMode = true;
				logger.info("Mathematica mode enabled");
			}
		}
		logger.info("Using dot executable: {}", edu.uci.ics.jung.contrib.DotLayout.dotProgram);
		logger.info("Using core executable: {}", CoreTalker.quantoCoreExecutable);

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception e) {
			logger.warn("Could not set look-and-feel", e);
		}

		if (QuantoApp.isMac && !mathematicaMode) {
			//System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty(
				"com.apple.mrj.application.apple.menu.about.name",
				"Quanto");
		}

		QuantoApp app = getInstance();

		app.newGraph(true);

		logger.info("Finished initialisation");
	}

	public boolean shutdown() {
		saveRulesetState();
		logger.info("Shutting down");
		if (viewManager.closeAllViews()) {
			logger.info("Exiting now");
			System.exit(0);
		}
		return false;
	}

	private void loadSavedRulesetState() {
		String ruleset = getPreference(SAVED_RULESET);
		if (ruleset.length() > 0) {
			logger.info("Existing theory state found: loading");
			try {
				core.loadRuleset(ruleset);
				return;
			} catch (Exception e) {
				logger.warn("Failed to load ruleset state", e);
			}
		}
		// FIXME: try loading default ruleset
	}

	private void saveRulesetState() {
		try {
			logger.info("Saving theory state");
			setPreference(SAVED_RULESET, core.getRulesetEncoded());
			return;
		} catch (Exception e) {
			logger.warn("Failed to save ruleset state", e);
		}
	}

	private QuantoApp() throws CoreException {
		globalPrefs = Preferences.userNodeForPackage(this.getClass());

		core = new Core();
		loadSavedRulesetState();
		viewManager = new InteractiveViewManager(this, core);

		if (MAC_OS_X) {
			try {
				OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("shutdown", (Class[]) null));
			}
			catch (Exception e) {
				logger.error("Could not set quit handler", e);
			}
		}
	}

	public JFileChooser getFileChooser() {
		if (fileChooser == null) {
			fileChooser = new JFileChooser();
		}
		return fileChooser;
	}

	public InteractiveViewManager getViewManager() {
		return viewManager;
	}

	public Core getCore() {
		return core;
	}

	public void errorDialog(String message) {
		JOptionPane.showMessageDialog(null, message, "Console Error", JOptionPane.ERROR_MESSAGE);
	}

	public void createNewFrame() {
		try {
			InteractiveView view = viewManager.getNextFreeView();
			if (view == null)
				view = createNewGraph();
			openNewFrame(view);
		}
		catch (CoreException ex) {
			logger.error("Could not create a new graph", ex);
			errorDialog("Could not create a new graph to display");
		}
	}

	public void openNewFrame(InteractiveView view)
		throws ViewUnavailableException
	{
		QuantoFrame fr = new QuantoFrame(this);
		try {
			fr.getViewPort().attachView(view);
			fr.pack();
			fr.setVisible(true);
		}
		catch (ViewUnavailableException ex) {
			logger.warn("Tried to open an already-attached view in a new frame", ex);
			fr.dispose();
			throw ex;
		}
	}

	public InteractiveGraphView createNewGraph()
		throws CoreException {
		QuantoGraph newGraph = core.createEmptyGraph();
		InteractiveGraphView vis =
			new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
		viewManager.addView(vis);
		return vis;
	}

	public InteractiveGraphView openGraph(File file)
		throws CoreException,
		       QuantoGraph.ParseException,
		       java.io.IOException {
		QuantoGraph loadedGraph = core.loadGraph(file);
		InteractiveGraphView vis =
			new InteractiveGraphView(core, loadedGraph, new Dimension(800, 600));
		vis.setTitle(file.getName());

		viewManager.addView(vis);
		core.renameGraph(loadedGraph, viewManager.getViewName(vis));

		vis.updateGraph();
		vis.getGraph().setSaved(true);
		return vis;
	}

	/**
	 * Create a new graph, read the name, and send to a fresh
	 * InteractiveQuantoVisualizer.
	 * @param initial   a <code>boolean</code> that tells whether this is the
	 *                  first call to newGraph().
	 */
	public void newGraph(boolean initial) {
		try {
			QuantoGraph newGraph = core.createEmptyGraph();
			InteractiveGraphView vis =
				new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
			viewManager.addView(vis);

			if (initial || getPreference(NEW_WINDOW_FOR_GRAPHS)) { // are we making a new window?
				openNewFrame(vis);
			}
		}
		catch (CoreException e) {
			logger.error("Failed to create a new graph", e);
			errorDialog(e.getMessage());
		}
	}

	public void newGraph() {
		newGraph(false);
	}

	/**
	 * Get a global preference. This method is overloaded because the preference API
	 * doesn't support generics.
	 */
	public boolean getPreference(QuantoApp.BoolPref pref) {
		return globalPrefs.getBoolean(pref.key, pref.def);
	}

	public String getPreference(QuantoApp.StringPref pref) {
		return globalPrefs.get(pref.key, pref.def);
	}

	/**
	 * Set a global preference.
	 */
	public void setPreference(QuantoApp.BoolPref pref, boolean value) {
		globalPrefs.putBoolean(pref.key, value);
	}

	public void setPreference(QuantoApp.StringPref pref, String value) {
		globalPrefs.put(pref.key, value);
	}
}
