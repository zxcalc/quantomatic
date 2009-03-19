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

import org.apache.commons.collections15.BidiMap;
import org.apache.commons.collections15.bidimap.DualHashBidiMap;

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
	
	
	public static final class Pref<T> implements ItemListener {
		final T def; // default value
		final String key;
		protected Pref(String key, T def) {
			this.key = key; this.def = def;
		}
		
		@SuppressWarnings("unchecked") // handling ClassCastException manually
		public void itemStateChanged(ItemEvent e) {
			try {
				QuantoApp.getInstance().setPreference
					((Pref<Boolean>)this, e.getStateChange()==ItemEvent.SELECTED);
			} catch (ClassCastException exp) {
				throw new QuantoCore.FatalError(
					"Attempted to use non-boolean pref as item listener.");
			}
		}
	}
	
	// Preferences
	public static final Pref<Boolean> DRAW_ARROW_HEADS =
		new Pref<Boolean>("draw_arrow_heads", false);
	public static final Pref<Boolean> NEW_WINDOW_FOR_GRAPHS =
		new Pref<Boolean>("new_window_for_graphs", false);
	public static final Pref<Boolean> CONSOLE_ECHO =
		new Pref<Boolean>("console_echo", false);
	
	
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
			}
		}
		
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.err.println("ERROR SETTING LOOK AND FEEL:");
			e.printStackTrace();
		}
		if (QuantoApp.isMac) {
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Quanto");
		}
		
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				getInstance().newGraph(true);
			}
		});
	}
	
	
	
	
	
	private QuantoApp() {
		globalPrefs = Preferences.userNodeForPackage(this.getClass());
		fileChooser = new JFileChooser();
		views = new DualHashBidiMap<String,InteractiveView>();
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
		public final JMenuItem file_saveRules;
		public final JMenuItem file_loadRules;
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
						focusedViewPort.setFocusedView(v);
						fr.pack();
						fr.setVisible(true);
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
			
			
			file_loadRules = new JMenuItem("Load Rule Set");
			file_loadRules.addActionListener(new ActionListener() { 
					public void actionPerformed(ActionEvent e) {
						QuantoApp.getInstance().loadRuleSet();
					}
			});
			fileMenu.add(file_loadRules);
			
			file_saveRules = new JMenuItem("Save Rule Set");
			file_saveRules.addActionListener(new ActionListener() { 
					public void actionPerformed(ActionEvent e) {
						QuantoApp.getInstance().saveRuleSet();
					}
			});
			fileMenu.add(file_saveRules);
			
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
		int retVal = fileChooser.showDialog(null, "Open");
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File f = fileChooser.getSelectedFile();
				String filename = f.getCanonicalPath().replaceAll("\\n|\\r", "");
				QuantoGraph loadedGraph = core.load_graph(filename);
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
	

	
	public void loadRuleSet() {
		int retVal = fileChooser.showDialog(null, "Open");
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try {
				String filename = fileChooser.getSelectedFile().getCanonicalPath().replaceAll("\\n|\\r", "");
				core.load_ruleset(filename);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
	}
	
	public void saveRuleSet() {
		int retVal = fileChooser.showSaveDialog(null);
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try{
				String filename = fileChooser.getSelectedFile().getCanonicalPath().replaceAll("\\n|\\r", "");
				core.save_ruleset(filename);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
	}

	public ViewPort getFocusedViewPort() {
		return focusedViewPort;
	}
	
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
	public boolean getPreference(QuantoApp.Pref<Boolean> pref) {
		return globalPrefs.getBoolean(pref.key, pref.def);
	}
	
	/**
	 * Set a global preference.
	 */
	public void setPreference(QuantoApp.Pref<Boolean> pref, boolean value) {
		globalPrefs.putBoolean(pref.key, value);
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

}
