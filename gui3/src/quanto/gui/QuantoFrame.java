package quanto.gui;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class QuantoFrame extends JFrame implements InteractiveView.Holder {
	private static final long serialVersionUID = 3656684775223085393L;
	protected QuantoCore core;
	protected QuantoConsole console;
	protected Map<String,InteractiveView> views;
	protected InteractiveView focusedView;
	boolean consoleVisible;
	final JTabbedPane tabs;
	
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	
	
	public QuantoFrame() {
		tabs = new JTabbedPane();
		consoleVisible = true;
		focusedView = null;
		
		tabs.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				setFocusedView((InteractiveView)tabs.getSelectedComponent());
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
		
		JMenuItem item = new JMenuItem("New Graph", KeyEvent.VK_N);
		item.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, modifierKey));
		fileMenu.add(item);
		
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
				synchronized (views) {
					for (InteractiveView v : views.values()) {
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
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, modifierKey | Event.ALT_MASK));
		viewMenu.add(item);
		
		mb.add(fileMenu);
		mb.add(viewMenu);
		
		
		setJMenuBar(mb);
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(tabs, BorderLayout.CENTER);
		
		views = new HashMap<String, InteractiveView>();
        
        console = new QuantoConsole();
        core = console.qcore;
        getContentPane().add(console, BorderLayout.NORTH);
        newGraph();
        this.pack();
        showHideConsole();
	}
	
	public void setFocusedView (InteractiveView v) {
		JMenuBar mb = getJMenuBar();
		if (focusedView != null) {
			for (JMenu m : focusedView.getMenus()) mb.remove(m);
		}
		focusedView = v;
		for (JMenu m : focusedView.getMenus()) mb.add(m);
		mb.repaint();
	}
	
	public void addView(InteractiveView iv) {
		iv.setViewHolder(this);
		views.put(iv.getTitle(), iv);
		tabs.add(iv.getTitle(), (Component)iv);
		tabs.setSelectedComponent((Component)iv);
	}
	
	/**
	 * Create a new graph, read the name, and send to a fresh
	 * InteractiveQuantoVisualizer. This method and
	 * QuantoConsole.updateGraphFromOutput() are the only methods
	 * that generate interactive (named) graphs.
	 * 
	 */
	public void newGraph() {
		try {
			QuantoGraph newGraph = core.new_graph();
			InteractiveGraphView vis =
				new InteractiveGraphView(core, newGraph, new Dimension(800,600));
			addView(vis);
		} catch (QuantoCore.ConsoleError e) {
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
		this.pack();
	}


	public static void main(String[] args) {
		if (QuantoFrame.isMac) {
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Quanto");
		}
		
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

	public QuantoCore getCore() {
		return core;
	}

}
