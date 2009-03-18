package quanto.gui;


import java.awt.Dimension;
import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Map;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
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
	private ConsoleView console;
	private QuantoCore core;
	public JFileChooser fileChooser;
	
	
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
		
		QuantoFrame fr = new QuantoFrame();
		getInstance().newGraph();
		fr.pack();
		fr.setVisible(true);
	}
	
	
	
	
	
	private QuantoApp() {
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
		public final JCheckBoxMenuItem view_verboseConsole;
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
			
			view_verboseConsole = new JCheckBoxMenuItem("Verbose console");
			view_verboseConsole.setMnemonic(KeyEvent.VK_V);
			view_verboseConsole.setSelected(QuantoApp.getInstance().getCore().getConsoleEcho());
			view_verboseConsole.addItemListener(new ItemListener() {
				public void itemStateChanged(ItemEvent e) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						QuantoApp.getInstance().getCore().setConsoleEcho(true);
					} else {
						QuantoApp.getInstance().getCore().setConsoleEcho(false);
					}
				}
			});
			viewMenu.add(view_verboseConsole);
			
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
				if (focusedViewPort != null) focusedViewPort.setFocusedView(v);
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
	 * 
	 */
	public void newGraph() {
		try {
			QuantoGraph newGraph = core.new_graph();
			InteractiveGraphView vis =
				new InteractiveGraphView(core, newGraph, new Dimension(800,600));
			String v = QuantoApp.getInstance().addView("new-graph",vis);
			if (focusedViewPort != null) focusedViewPort.setFocusedView(v);
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	

	
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
				if (! ent.getValue().hasParent()) return ent.getKey();
			}
		}
		return null;
	}


}
