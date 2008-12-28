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


public class QuantoFrame extends JFrame {
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
				if (tabs.getSelectedComponent() instanceof
						InteractiveQuantoVisualizer) {
					setFocusedView(
							(InteractiveQuantoVisualizer)
							tabs.getSelectedComponent());
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
			// TODO: make it work
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
		mb.add(fileMenu);
		mb.add(viewMenu);
		
		
		setJMenuBar(mb);
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(tabs, BorderLayout.CENTER);
		
		views = new HashMap<String, InteractiveView>();
        
        console = new QuantoConsole(tabs, views);
        core = console.qcore;
        getContentPane().add(console, BorderLayout.NORTH);
        newGraph();
        this.pack();
        showHideConsole();
	}
	
	public void setFocusedView (InteractiveQuantoVisualizer v) {
		JMenuBar mb = getJMenuBar();
		if (focusedView != null) {
			for (JMenu m : focusedView.getMenus()) mb.remove(m);
		}
		focusedView = v;
		for (JMenu m : focusedView.getMenus()) mb.add(m);
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
			InteractiveQuantoVisualizer vis =
				new InteractiveQuantoVisualizer(core, newGraph, new Dimension(800,600));
			//vis.updateGraph();
			views.put(newGraph.getName(), vis);
			tabs.add(newGraph.getName(), vis);
			tabs.setSelectedIndex(tabs.indexOfComponent(vis));
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	
	public QuantoGraph getCurrentGraph() {
		if (focusedView != null &&
				focusedView instanceof InteractiveQuantoVisualizer) {
			return ((InteractiveQuantoVisualizer)focusedView).getGraph();
		} else {
			return null;
		}
	}
	
	public void updateCurrentGraph() throws QuantoCore.ConsoleError {
		if (focusedView != null &&
				focusedView instanceof InteractiveQuantoVisualizer) {
			((InteractiveQuantoVisualizer)focusedView).updateGraph();
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
