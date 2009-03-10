package quanto.gui;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class QuantoFrame extends JFrame implements InteractiveView.Holder {
	private static final long serialVersionUID = 3656684775223085393L;
	private QuantoCore core;
	private QuantoConsole console;
	private InteractiveView focusedView;
	private JMenuItem closeViewMenuItem, saveGraphAsMenuItem, saveGraphMenuItem;

	private Object viewLock = new Object();
	private JFileChooser fileChooser = new JFileChooser();
	
	boolean consoleVisible;
	final JTabbedPane tabs;
	
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	public static boolean appMode;
	
	public QuantoFrame() {
		setBackground(Color.white);
		tabs = new JTabbedPane();
		consoleVisible = true;
		focusedView = null;
		
		console = new QuantoConsole();
        core = console.getCore();
		tabs.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				setFocusedView((InteractiveView)tabs.getSelectedComponent());
				
				// if there are no views open, ghost commands for closing and saving
				if (focusedView == null) {
					closeViewMenuItem.setEnabled(false);
					saveGraphAsMenuItem.setEnabled(false);
					saveGraphMenuItem.setEnabled(false);
				}
				else {
					closeViewMenuItem.setEnabled(true);
					saveGraphAsMenuItem.setEnabled(true);
					focusedView.gainFocus();
				}
			}
		});
		
		int modifierKey;
	    if (QuantoFrame.isMac) modifierKey = Event.META_MASK;
	    else modifierKey = Event.CTRL_MASK;
		
		JMenuBar mb = new JMenuBar();
		JMenu fileMenu = new JMenu("File");
		JMenu viewMenu = new JMenu("View");
		
		fileMenu.setMnemonic(KeyEvent.VK_F);
		viewMenu.setMnemonic(KeyEvent.VK_V);
		
		// new graph
		JMenuItem item = new JMenuItem("New Graph", KeyEvent.VK_N);
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, modifierKey));
		fileMenu.add(item);
		
		
		// open graph
		item = new JMenuItem("Open Graph...", KeyEvent.VK_O);
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, modifierKey));
		fileMenu.add(item);
		
		// Save Graph
		saveGraphMenuItem = new JMenuItem("Save Graph", KeyEvent.VK_S);
		saveGraphMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveGraph();
			}
		});
		saveGraphMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, modifierKey));
		fileMenu.add(saveGraphMenuItem);
		
		// Save Graph As
		saveGraphAsMenuItem = new JMenuItem("Save Graph As...", KeyEvent.VK_A);
		saveGraphAsMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveGraphAs();
			}
		});
		saveGraphAsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, modifierKey | Event.SHIFT_MASK));
		fileMenu.add(saveGraphAsMenuItem);
		
		// load and save rule sets
		JMenuItem loadrules = new JMenuItem("Load Rule Set");
		loadrules.addActionListener(new ActionListener() { 
				public void actionPerformed(ActionEvent e) {
					loadRuleSet();
				}
		});
		fileMenu.add(loadrules);
		
		JMenuItem saverules = new JMenuItem("Save Rule Set");
		saverules.addActionListener(new ActionListener() { 
				public void actionPerformed(ActionEvent e) {
					saveRuleSet();
				}
		});
		fileMenu.add(saverules);
		
		// quit
		if (!isMac) {
			item = new JMenuItem("Quit", KeyEvent.VK_Q);
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					// TODO: close better?
					System.exit(0);
				}
			});
			item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, modifierKey));
			fileMenu.add(item);
		}
		
		item = new JMenuItem("Show/Hide Console", KeyEvent.VK_C);
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showHideConsole();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_BACK_QUOTE, modifierKey));
		viewMenu.add(item);
		
		
		
		item = new JMenuItem("Refresh All Graphs", KeyEvent.VK_R);
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				synchronized (viewLock) {
					for (Component cmp : tabs.getComponents()) {
						InteractiveView v = (InteractiveView)cmp;
						if (v instanceof InteractiveGraphView) {
							try {
								((InteractiveGraphView)v).updateGraph();
							} catch (QuantoCore.ConsoleError err) {
								errorDialog(err.getMessage());
							}
						}
					}
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, modifierKey | Event.SHIFT_MASK));
		viewMenu.add(item);
		
		JCheckBoxMenuItem cbItem = new JCheckBoxMenuItem("Verbose console");
		cbItem.setMnemonic(KeyEvent.VK_V);
		cbItem.setSelected(getCore().getConsoleEcho());
		cbItem.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					getCore().setConsoleEcho(true);
				} else {
					getCore().setConsoleEcho(false);
				}
			}
		});
		viewMenu.add(cbItem);
		
		closeViewMenuItem = new JMenuItem("Close Current View", KeyEvent.VK_W);
		closeViewMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				closeView(focusedView);
			}
		});
		closeViewMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, modifierKey));
		viewMenu.add(closeViewMenuItem);
		
		mb.add(fileMenu);
		mb.add(viewMenu);
		
		
		setJMenuBar(mb);
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(tabs, BorderLayout.CENTER);
        
        getContentPane().add(console, BorderLayout.NORTH);
        newGraph();
        this.pack();
        showHideConsole();
	}
	
	public void setFocusedView (InteractiveView v) {
		synchronized (viewLock) {
			JMenuBar mb = getJMenuBar();
			if (focusedView != null) {
				for (JMenu m : focusedView.getMenus()) mb.remove(m);
			}
			focusedView = v;
			if (focusedView != null) {
				for (JMenu m : focusedView.getMenus()) mb.add(m);
			}
			
			mb.repaint();
		}
	}
	
	public void addView(InteractiveView iv) {
		synchronized (viewLock) {
			iv.setViewHolder(this);
			tabs.add(iv.getTitle(), (Component)iv);
			tabs.setSelectedComponent((Component)iv);
		}
		pack();
		iv.gainFocus();
	}
	
	public void closeView(InteractiveView iv) {
		synchronized (viewLock) {
			tabs.remove((Component)iv);
		}
	}
	
	/**
	 * Create a new graph, read the name, and send to a fresh
	 * InteractiveQuantoVisualizer. This method, openGraph(), and
	 * QuantoConsole.updateGraphFromOutput() are the only methods
	 * that generate interactive (named) graphs.
	 * 
	 */
	public void newGraph() {
		try {
			QuantoGraph newGraph = core.new_graph();
			InteractiveGraphView vis =
				new InteractiveGraphView(core, newGraph, new Dimension(800,600),saveGraphMenuItem);
			addView(vis);
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	
	/** 
	 * Read a graph from a file and send it to a fresh InteractiveGraphView.
	 */
	public void openGraph() {
		int retVal = fileChooser.showDialog(this, "Open");
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try {
				String filename = fileChooser.getSelectedFile().getCanonicalPath().replaceAll("\\n|\\r", "");
				QuantoGraph loadedGraph = core.load_graph(filename);
				InteractiveGraphView vis =
					new InteractiveGraphView(core, loadedGraph, new Dimension(800,600),saveGraphMenuItem);
				addView(vis);
				vis.getGraph().setFileName(filename);
				vis.getGraph().setSaved(true);
				vis.updateGraph();
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
	}
	
	public void saveGraphAs() {
		int retVal = fileChooser.showSaveDialog(this);
		if(retVal == JFileChooser.APPROVE_OPTION) {
			try{
				String filename = fileChooser.getSelectedFile().getCanonicalPath().replaceAll("\\n|\\r", "");
				core.save_graph(getCurrentGraph(), filename);
				getCurrentGraph().setFileName(filename);
				getCurrentGraph().setSaved(true);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch(java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
	}
	
	public void loadRuleSet() {
		int retVal = fileChooser.showDialog(this, "Open");
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
		int retVal = fileChooser.showSaveDialog(this);
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
	
	
	
	public void saveGraph() {
		try {
			getCore().save_graph(getCurrentGraph(), getCurrentGraph().getFileName());
			getCurrentGraph().setSaved(true);
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}

	
	public QuantoGraph getCurrentGraph() {
		if (focusedView != null &&
				focusedView instanceof InteractiveGraphView) {
			return ((InteractiveGraphView)focusedView).getGraph();
		} else {
			return null;
		}
	}
	
	public void updateCurrentGraph() throws QuantoCore.ConsoleError {
		if (focusedView != null &&
				focusedView instanceof InteractiveGraphView) {
			((InteractiveGraphView)focusedView).updateGraph();
		}
	}
	
	public void errorDialog(String message) {
		JOptionPane.showMessageDialog(this, message, "Console Error", JOptionPane.ERROR_MESSAGE);
	}
	
	public void showHideConsole() {
		consoleVisible = !consoleVisible;
		console.setVisible(consoleVisible);
		if (consoleVisible) console.grabFocus();
		pack();
	}

	public QuantoCore getCore() {
		return core;
	}

	public static void main(String[] args) {
		appMode = false;
		for (String arg : args) {
			if (arg.equals("--app-mode")) {
				appMode = true;
				QuantoCore.appMode = true;
				System.out.println("Invoked as OS X application.");
				System.out.printf("Working dir is %s\n",
						System.getProperty("user.dir"));
			}
		}
		
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.err.println("ERROR SETTING LOOK AND FEEL:");
			e.printStackTrace();
		}
		if (QuantoFrame.isMac) {
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Quanto");
		}
		
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

	

}
