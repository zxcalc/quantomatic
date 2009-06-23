package quanto.gui;


import java.awt.Component;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.StringWriter;
import java.util.Map;
import java.util.prefs.Preferences;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.XMLWriter;

import org.apache.commons.collections15.BidiMap;
import org.apache.commons.collections15.bidimap.DualTreeBidiMap;
import org.apache.commons.collections15.comparators.ComparableComparator;
import org.apache.commons.collections15.contrib.HashCodeComparator;

import edu.uci.ics.jung.contrib.HasName;

/**
 * Singleton class 
 * @author aleks
 *
 */
public class QuantoApp {
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	private static QuantoApp theApp = null;
	
	
	private static class Pref<T> {
		final T def; // default value
		final String key;
		protected Pref(String key, T def) {
			this.key = key; this.def = def;
		}
	}
	
	public static class StringPref extends Pref<String> {
		protected StringPref(String key, String def) {super(key,def);} 
	}
	
	public static class BoolPref extends Pref<Boolean> implements ItemListener {
		protected BoolPref(String key, Boolean def) {super(key, def);}
		
		public void itemStateChanged(ItemEvent e) {
			try {
				QuantoApp.getInstance().setPreference
					(this, e.getStateChange()==ItemEvent.SELECTED);
			} catch (ClassCastException exp) {
				throw new QuantoCore.FatalError(
					"Attempted to use non-boolean pref as item listener.");
			}
		}
	}
	
	// Preferences
	public static final BoolPref DRAW_ARROW_HEADS =
		new BoolPref("draw_arrow_heads", false);
	public static final BoolPref NEW_WINDOW_FOR_GRAPHS =
		new BoolPref("new_window_for_graphs", false);
	public static final BoolPref CONSOLE_ECHO =
		new BoolPref("console_echo", false);
	public static final StringPref LAST_OPEN_DIR =
		new StringPref("last_open_dir", null);
	public static final StringPref LAST_THEORY_OPEN_DIR =
		new StringPref("last_theory_open_dir", null);
	public static final StringPref LOADED_THEORIES =
		new StringPref("loaded_theories", "");
	public static final StringPref ACTIVE_THEORIES =
		new StringPref("loaded_theories", "");
	
	private final Preferences globalPrefs;
	private final ConsoleView console;
	private final QuantoCore core;
	public final JFileChooser fileChooser;
	private final BidiMap<String,InteractiveView> views;
	private volatile ViewPort focusedViewPort = null;
	
	public static QuantoApp getInstance() {
		if (theApp == null) theApp = new QuantoApp();
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
		for (String arg : args) {
			if (arg.equals("--app-mode")) {
				QuantoCore.appMode = true;
				edu.uci.ics.jung.contrib.DotLayout.dotProgram =
					"Quantomatic.app/Contents/MacOS/dot_static";
				System.out.println("Invoked as OS X application.");
			} else if (arg.equals("--mathematica-mode")) {
				QuantoCore.mathematicaMode = true;
			}
		}
		
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.err.println("ERROR SETTING LOOK AND FEEL:");
			e.printStackTrace();
		}
		
		if (QuantoApp.isMac && !QuantoCore.mathematicaMode) {	
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Quanto");
		}
		
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				getInstance().newGraph(true);
				TheoryTree.loadState();
			}
		});
	}
	
	private QuantoApp() {
		globalPrefs = Preferences.userNodeForPackage(this.getClass());
		fileChooser = new JFileChooser();
		
		// bidirectional map implemented as dual trees. note that get(null) or
		//  getKey(null) will raise exceptions in the Comparators.
		views = new DualTreeBidiMap<String, InteractiveView>(
				ComparableComparator.<String>getInstance(),
				new HashCodeComparator<InteractiveView>());
		console = new ConsoleView();
		core = console.getCore();
		addView("console", console);
	}
	
	public String addView(String name, InteractiveView v) {
		String realName = HasName.StringNamer.getFreshName(views.keySet(), name);
		//System.out.printf("adding %s\n", realName);
		synchronized (views) {views.put(realName, v);}
		return realName;
	}
	
	public String getViewName(InteractiveView v) {
		return views.getKey(v);
	}
	
	public String renameView(String oldName, String newName) {
		String realNewName;
		synchronized (views) {
			InteractiveView v = views.get(oldName);
			if (v == null) throw new QuantoCore.FatalError("Attempting to rename null view.");
			views.remove(oldName);
			realNewName = addView(newName, v);
			if (focusedViewPort != null) {
				if (focusedViewPort.getFocusedView().equals(oldName))
					focusedViewPort.setFocusedView(realNewName);
			}
		}
		return realNewName;
	}
	
	public String renameView(InteractiveView v, String newName) {
		return renameView(getViewName(v), newName);
	}
	
	public Map<String,InteractiveView> getViews() {
		return views;
	}

	public void removeView(String name) {
		synchronized (views) {views.remove(name);}
	}
	
	public MainMenu getMainMenu() {
		return new MainMenu();
	}
	
	public class MainMenu extends JMenuBar {
		private static final long serialVersionUID = 1L;
		public final JMenu fileMenu;
		public final JMenu viewMenu;
		private final JCheckBoxMenuItem view_newWindowForGraphs;
		public final JCheckBoxMenuItem view_verboseConsole;
		public final JCheckBoxMenuItem view_drawArrowHeads;
		public final JMenuItem view_refreshAllGraphs;
		public final JMenuItem file_quit;
		public final JMenuItem file_saveTheory;
		public final JMenuItem file_loadTheory;
		public final JMenuItem file_openGraph;
		public final JMenuItem file_newGraph;
		public final JMenuItem file_newWindow;
		
		private int getIndexOf(JMenu m, JMenuItem mi) {
			for (int i=0; i<m.getItemCount(); i++)
				if (m.getItem(i).equals(mi)) return i;
			throw new QuantoCore.FatalError(
					"Attempted getIndexOf() for non-existent menu item.");
		}
		
		public void insertBefore(JMenu m, JMenuItem before, JMenuItem item) {
			m.insert(item, getIndexOf(m, before));
		}
		
		public void insertAfter(JMenu m, JMenuItem after, JMenuItem item) {
			m.insert(item, getIndexOf(m, after)+1);
		}
		
		
		public MainMenu() {
			int commandMask;
		    if (QuantoApp.isMac) commandMask = Event.META_MASK;
		    else commandMask = Event.CTRL_MASK;
		    
			fileMenu = new JMenu("File");
			viewMenu = new JMenu("View");
			fileMenu.setMnemonic(KeyEvent.VK_F);
			viewMenu.setMnemonic(KeyEvent.VK_V);
			
			file_newGraph = new JMenuItem("New Graph", KeyEvent.VK_G);
			file_newGraph.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					QuantoApp.getInstance().newGraph();
				}
			});
			file_newGraph.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, commandMask));
			fileMenu.add(file_newGraph);
			
			file_newWindow = new JMenuItem("New Window", KeyEvent.VK_N);
			file_newWindow.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String v = getFirstFreeView();
					if (v!=null) {
						QuantoFrame fr = new QuantoFrame();
						fr.setVisible(true);
						fr.getViewPort().setFocusedView(v);
						fr.pack();
					} else {
						errorDialog("no more views to show");
					}
				}
			});
			file_newWindow.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_N, commandMask | KeyEvent.SHIFT_MASK));
			fileMenu.add(file_newWindow);
			
			
			file_openGraph = new JMenuItem("Open Graph...", KeyEvent.VK_O);
			file_openGraph.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					QuantoApp.getInstance().openGraph();
				}
			});
			file_openGraph.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, commandMask));
			fileMenu.add(file_openGraph);
			
			
			file_loadTheory = new JMenuItem("Load Theory...");
			file_loadTheory.addActionListener(new ActionListener() { 
					public void actionPerformed(ActionEvent e) {
						QuantoApp.getInstance().loadTheory();
					}
			});
			fileMenu.add(file_loadTheory);
			
			file_saveTheory = new JMenuItem("Save Theory");
			file_saveTheory.addActionListener(new ActionListener() { 
					public void actionPerformed(ActionEvent e) {
						System.err.println("SAVE NOT IMPLEMENTED");
//						QuantoApp.getInstance().saveRuleSet();
					}
			});
			file_saveTheory.setEnabled(false);
			fileMenu.add(file_saveTheory);
			
			// quit
			if (!isMac) {
				file_quit = new JMenuItem("Quit", KeyEvent.VK_Q);
				file_quit.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						// TODO: close better?
						System.exit(0);
					}
				});
				file_quit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, commandMask));
				fileMenu.add(file_quit);
			} else {
				file_quit = null;
			}
			
			view_refreshAllGraphs = new JMenuItem("Refresh All Graphs", KeyEvent.VK_R);
			view_refreshAllGraphs.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					synchronized (QuantoApp.getInstance().getViews()) {
						for (InteractiveView v : QuantoApp.getInstance().getViews().values()) {
							if (v instanceof InteractiveGraphView) {
								try {
									((InteractiveGraphView)v).updateGraph();
								} catch (QuantoCore.ConsoleError err) {
									QuantoApp.getInstance().errorDialog(err.getMessage());
								}
							}
						}
					}
				}
			});
			view_refreshAllGraphs.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, commandMask | Event.SHIFT_MASK));
			viewMenu.add(view_refreshAllGraphs);
			
			viewMenu.addSeparator();
			
			view_drawArrowHeads = new JCheckBoxMenuItem("Draw Arrow Heads");
			view_drawArrowHeads.setSelected(
					QuantoApp.getInstance().getPreference(QuantoApp.DRAW_ARROW_HEADS));
			view_drawArrowHeads.addItemListener(QuantoApp.DRAW_ARROW_HEADS);
			viewMenu.add(view_drawArrowHeads);
			
			view_verboseConsole = new JCheckBoxMenuItem("Verbose Console");
			view_verboseConsole.setSelected(
					QuantoApp.getInstance().getPreference(QuantoApp.CONSOLE_ECHO));
			view_verboseConsole.addItemListener(QuantoApp.CONSOLE_ECHO);
			viewMenu.add(view_verboseConsole);
			
			view_newWindowForGraphs = new JCheckBoxMenuItem("Open Graphs in a New Window");
			view_newWindowForGraphs.setSelected(
					QuantoApp.getInstance().getPreference(QuantoApp.NEW_WINDOW_FOR_GRAPHS));
			view_newWindowForGraphs.addItemListener(QuantoApp.NEW_WINDOW_FOR_GRAPHS);
			viewMenu.add(view_newWindowForGraphs);
			
	//		closeViewMenuItem = new JMenuItem("Close Current View", KeyEvent.VK_W);
	//		closeViewMenuItem.addActionListener(new ActionListener() {
	//			public void actionPerformed(ActionEvent e) {
	//				//closeView(focusedView);
	//			}
	//		});
	//		closeViewMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, modifierKey));
	//		viewMenu.add(closeViewMenuItem);
			
			add(fileMenu);
			add(viewMenu);
		}
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
			} catch (QuantoCore.ConsoleError err) {
				JOptionPane.showMessageDialog(
						parent,
						err.getMessage(),
						"Console Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
		
		public abstract void wrappedAction(ActionEvent e) throws QuantoCore.ConsoleError;
	}


	public ConsoleView getConsole() {
		return console;
	}

	public QuantoCore getCore() {
		return core;
	}
	
	public void errorDialog(String message) {
		JOptionPane.showMessageDialog(null, message, "Console Error", JOptionPane.ERROR_MESSAGE);
	}
	
	/** 
	 * Read a graph from a file and send it to a fresh InteractiveGraphView.
	 */
	public void openGraph() {
		String lastDir = getPreference(LAST_OPEN_DIR);
		if (lastDir != null) fileChooser.setCurrentDirectory(new File(lastDir));
		
		int retVal = fileChooser.showDialog(null, "Open");
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File f = fileChooser.getSelectedFile();
				if (f.getParent()!=null) setPreference(LAST_OPEN_DIR, f.getParent());
				String filename = f.getCanonicalPath().replaceAll("\\n|\\r", "");
				QuantoGraph loadedGraph = new QuantoGraph();
				IXMLElement root = loadedGraph.fromXml(f);
				StringWriter sw = new StringWriter();
				new XMLWriter(sw).write(root, true);
				loadedGraph.setName(core.input_graph_xml(sw.toString()));
				InteractiveGraphView vis =
					new InteractiveGraphView(core, loadedGraph, new Dimension(800,600));
				vis.getGraph().setFileName(filename);
				vis.getGraph().setSaved(true);
				vis.updateGraph();
				String v = addView(f.getName(), vis);
				if (getPreference(NEW_WINDOW_FOR_GRAPHS)) { // in a new window?
					QuantoFrame fr = new QuantoFrame();
					fr.getViewPort().setFocusedView(v);
					fr.pack();
					fr.setVisible(true);
				} else if (focusedViewPort != null) { // otherwise force re-focus of active view with gainFocus()
					focusedViewPort.setFocusedView(v);
					focusedViewPort.gainFocus();
				}
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
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
				new InteractiveGraphView(core, newGraph, new Dimension(800,600));
			String v = QuantoApp.getInstance().addView("new-graph",vis);
			
			if (initial || getPreference(NEW_WINDOW_FOR_GRAPHS)) { // are we making a new window?
				QuantoFrame fr = new QuantoFrame();
				fr.getViewPort().setFocusedView(v);
				fr.getViewPort().gainFocus();
				fr.pack();
				fr.setVisible(true);
			} else if (focusedViewPort != null) { // if not, force the active view to focus with gainFocus()
				focusedViewPort.setFocusedView(v);
				focusedViewPort.gainFocus();
			}
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	public void newGraph() { newGraph(false); }
	

	
	public void loadTheory() {
		String lastDir = getPreference(LAST_THEORY_OPEN_DIR);
		if (lastDir != null) fileChooser.setCurrentDirectory(new File(lastDir));
		
		int retVal = fileChooser.showDialog(null, "Open");
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File file = fileChooser.getSelectedFile();
				if (file.getParent()!=null) setPreference(LAST_THEORY_OPEN_DIR, file.getParent());
				String thyname = file.getName().replaceAll("\\.xml|\\n|\\r", "");
				String filename = file.getCanonicalPath().replaceAll("\\n|\\r", "");
				TheoryTree.loadTheory(thyname, filename);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			} finally {
				TheoryTree.refreshInstances();
			}
		}
	}
	
//	public void saveRuleSet() {
//		int retVal = fileChooser.showSaveDialog(null);
//		if(retVal == JFileChooser.APPROVE_OPTION) {
//			try{
//				String filename = fileChooser.getSelectedFile().getCanonicalPath().replaceAll("\\n|\\r", "");
//				core.save_ruleset(filename);
//			}
//			catch (QuantoCore.ConsoleError e) {
//				errorDialog(e.getMessage());
//			}
//			catch(java.io.IOException ioe) {
//				errorDialog(ioe.getMessage());
//			}
//		}
//	}

	/**
	 * Get the currently focused viewport.
	 * @return
	 */
	public ViewPort getFocusedViewPort() {
		return focusedViewPort;
	}
	
	/**
	 * Set the focused view port and call the relevant focus handlers.
	 * @param vp
	 */
	public void setFocusedViewPort(ViewPort vp) {
		if (vp != focusedViewPort) {
			if (focusedViewPort!=null) focusedViewPort.loseFocus();
			focusedViewPort = vp;
			if (focusedViewPort!=null) focusedViewPort.gainFocus();
		}
	}
	
	/**
	 * return the first InteractiveGraphView available, or null.
	 * @return
	 */
	public String getFirstFreeView() {
		synchronized (views) {
			for (Map.Entry<String, InteractiveView> ent : views.entrySet()) {
				if (! ent.getValue().viewHasParent()) return ent.getKey();
			}
		}
		return null;
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
	
	/**
	 * Call "repaint" on all views that might be visible
	 */
	public void repaintViews() {
		synchronized (views) {
			for (InteractiveView v : views.values()) {
				if (v instanceof Component) ((Component)v).repaint();
			}
		}
	}
	
	public GraphView newGraphViewFromName(String name) {
		try {
			QuantoGraph gr = new QuantoGraph(name);
			gr.fromXml(getCore().graph_xml(gr));
			return new GraphView(gr);
		} catch (QuantoCore.ConsoleError e) {
			System.err.print(e.getMessage());
		}
		return null;
	}

}
