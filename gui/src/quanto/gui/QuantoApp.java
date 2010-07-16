// vim:sts=8:ts=8:noet:sw=8
package quanto.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.StringWriter;
import java.util.prefs.Preferences;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.XMLWriter;


import apple.dts.samplecode.osxadapter.OSXAdapter;
/**
 * Singleton class 
 * @author aleks
 *
 */
public class QuantoApp {
	// isMac is used for CTRL vs META shortcuts, etc

	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	// MAC_OS_X is used to determine whether we use OSXAdapter to
	// hook into the application menu
	public static boolean MAC_OS_X = (System.getProperty("os.name").toLowerCase().startsWith("mac os x"));
	public static int COMMAND_MASK =
		MAC_OS_X ? java.awt.event.InputEvent.META_DOWN_MASK
		         : java.awt.event.InputEvent.CTRL_DOWN_MASK;
	private static QuantoApp theApp = null;
	public static boolean useExperimentalLayout = false;

	private final Preferences globalPrefs;
	private final QuantoCore core;
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
			try {
				QuantoApp.getInstance().setPreference(this, e.getStateChange() == ItemEvent.SELECTED);
			}
			catch (ClassCastException exp) {
				throw new QuantoCore.FatalError(
					"Attempted to use non-boolean pref as item listener.");
			}
		}
	}
	// Preferences
	public static final BoolPref DRAW_ARROW_HEADS =
		new BoolPref("draw_arrow_heads", false, "Draw arrow geads");
	public static final BoolPref NEW_WINDOW_FOR_GRAPHS =
		new BoolPref("new_window_for_graphs", false, "Open graphs in a new window");
	public static final BoolPref CONSOLE_ECHO =
		new BoolPref("console_echo", false, "Verbose console");
	public static final BoolPref SHOW_INTERNAL_NAMES =
		new BoolPref("show_internal_names", false, "Show internal graph names");
	public static final StringPref LAST_OPEN_DIR =
		new StringPref("last_open_dir", null);
	public static final StringPref LAST_THEORY_OPEN_DIR =
		new StringPref("last_theory_open_dir", null);
	public static final StringPref LOADED_THEORIES =
		new StringPref("loaded_theories", "");

	public static QuantoApp getInstance() {
		if (theApp == null) {
			theApp = new QuantoApp();
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
		System.out.println("Initialised...");
		boolean mathematicaMode = false;
		for (String arg : args) {
			if (arg.equals("--app-mode")) {
				String appName = "Quantomatic.app";

				// determine the app name from the classpath if I can...
				System.out.println(System.getProperty("java.class.path"));
				for (String path : System.getProperty("java.class.path").split(System.getProperty("path.separator"))) {
					if (path.indexOf("QuantoGui.jar") != -1) {
						String[] dirs = path.split(System.getProperty("file.separator"));
						if (dirs.length >= 5) {
							appName = dirs[dirs.length - 5];
						}
					}
				}

				edu.uci.ics.jung.contrib.DotLayout.dotProgram =
					appName + "/Contents/MacOS/dot_static";
				QuantoCore.quantoCoreExecutable =
					appName + "/Contents/MacOS/quanto-core-app";
				System.out.println("Invoked as OS X application (" + appName + ")");
			}
			else if (arg.equals("--mathematica-mode")) {
				mathematicaMode = true;
			}
		}

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception e) {
			System.err.println("ERROR SETTING LOOK AND FEEL:");
			e.printStackTrace();
		}

		if (QuantoApp.isMac && !mathematicaMode) {
			//System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty(
				"com.apple.mrj.application.apple.menu.about.name",
				"Quanto");
		}

		QuantoApp app = getInstance();

		app.newGraph(true);

		System.out.println("loading theory...");

		TheoryTree.loadState(app.getCore(), app.getPreference(QuantoApp.LOADED_THEORIES));

		System.out.println("done.");
	}

	public boolean shutdown() {
		System.out.println("Shutting down...");
		if (viewManager.closeAllViews()) {
			System.exit(0);
		}
		return false;
	}

	private QuantoApp() {
		globalPrefs = Preferences.userNodeForPackage(this.getClass());

		viewManager = new InteractiveViewManager();
		core = viewManager.getConsole().getCore();

		if (MAC_OS_X) {
			try {
				OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("shutdown", (Class[]) null));
			}
			catch (SecurityException e) {
				throw new QuantoCore.FatalError(e);
			}
			catch (NoSuchMethodException e) {
				throw new QuantoCore.FatalError(e);
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

	/**
	 * Generic action listener that reports errors to a dialog box and gives
	 * actions access to the frame, console, and core.
	 */
	public static abstract class QuantoActionListener implements ActionListener {

		private Component parent;

		public QuantoActionListener(Component parent) {
			this.parent = parent;
		}

		public void actionPerformed(ActionEvent e) {
			try {
				wrappedAction(e);
			}
			catch (QuantoCore.ConsoleError err) {
				JOptionPane.showMessageDialog(
					parent,
					err.getMessage(),
					"Console Error",
					JOptionPane.ERROR_MESSAGE);
			}
		}

		public abstract void wrappedAction(ActionEvent e) throws QuantoCore.ConsoleError;
	}

	public QuantoCore getCore() {
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
		catch (QuantoCore.ConsoleError ex) {
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
			fr.dispose();
			throw ex;
		}
	}

	public InteractiveGraphView createNewGraph()
		throws QuantoCore.ConsoleError {
		QuantoGraph newGraph = core.new_graph();
		InteractiveGraphView vis =
			new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
		viewManager.addView(vis);
		return vis;
	}

	public InteractiveGraphView openGraph(File file)
		throws QuantoCore.ConsoleError,
		       QuantoGraph.ParseException,
		       java.io.IOException{
		String filename = file.getCanonicalPath().replaceAll("\\n|\\r", "");
		QuantoGraph loadedGraph = new QuantoGraph();
		IXMLElement root = loadedGraph.fromXml(file);
		StringWriter sw = new StringWriter();
		new XMLWriter(sw).write(root, true);
		loadedGraph.setName(core.input_graph_xml(sw.toString()));
		InteractiveGraphView vis =
			new InteractiveGraphView(core, loadedGraph, new Dimension(800, 600));
		vis.getGraph().setFileName(filename);
		vis.setTitle(file.getName());

		viewManager.addView(vis);
		core.rename_graph(loadedGraph, viewManager.getViewName(vis));

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
			QuantoGraph newGraph = core.new_graph();
			InteractiveGraphView vis =
				new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
			viewManager.addView(vis);

			if (initial || getPreference(NEW_WINDOW_FOR_GRAPHS)) { // are we making a new window?
				openNewFrame(vis);
			}
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}

	public void newGraph() {
		newGraph(false);
	}

	public void loadRuleset() {
		String lastDir = getPreference(LAST_THEORY_OPEN_DIR);
		if (lastDir != null) {
			getFileChooser().setCurrentDirectory(new File(lastDir));
		}

		int retVal = getFileChooser().showDialog(null, "Open");
		if (retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File file = getFileChooser().getSelectedFile();
				if (file.getParent() != null) {
					setPreference(LAST_THEORY_OPEN_DIR, file.getParent());
				}
				String thyname = file.getName().replaceAll("\\.theory|\\n|\\r", "");
				String filename = file.getCanonicalPath().replaceAll("\\n|\\r", "");
				TheoryTree.loadRuleset(getCore(), thyname, filename);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch (java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
			finally {
				TheoryTree.refreshInstances();
			}
		}
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
