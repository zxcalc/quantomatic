package quanto.gui;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;

import quanto.gui.QuantoCore.ConsoleError;

public class QuantoFrame extends JFrame {
	private static final long serialVersionUID = 3656684775223085393L;
	protected QuantoCore core;
	protected QuantoConsole console;
	boolean consoleVisible;
	final JTabbedPane tabs;
	
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	
	/**
	 * Generic action listener that reports errors to a dialog box and gives
	 * actions access to the frame, console, and core.
	 */
	protected abstract class QuantoFrameListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			try {
				wrappedAction(e);
			} catch (QuantoCore.ConsoleError err) {
				errorDialog(err.getMessage());
			}
		}
		
		public abstract void wrappedAction(ActionEvent e) throws QuantoCore.ConsoleError;
	}
	
	public QuantoFrame() {
		tabs = new JTabbedPane();
		consoleVisible = true;
		
		JMenuBar mb = new JMenuBar();
		JMenu graphMenu = new JMenu("Graph");
		JMenu viewMenu = new JMenu("View");
		
		graphMenu.setMnemonic(KeyEvent.VK_G);
		viewMenu.setMnemonic(KeyEvent.VK_V);
		
		int modifierKey;
	    if (isMac) modifierKey = Event.META_MASK;
	    else modifierKey = Event.CTRL_MASK;
		
		
		JMenuItem item = new JMenuItem("New Graph", KeyEvent.VK_N);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				newGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, modifierKey));
		graphMenu.add(item);
		
		JMenu graphAddMenu = new JMenu("Add");
		item = new JMenuItem("Red Vertex", KeyEvent.VK_R);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getCurrentGraphName(), QVertex.Type.RED);
				updateCurrentGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, modifierKey));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Green Vertex", KeyEvent.VK_G);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getCurrentGraphName(), QVertex.Type.GREEN);
				updateCurrentGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, modifierKey));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Boundary Vertex", KeyEvent.VK_B);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getCurrentGraphName(), QVertex.Type.BOUNDARY);
				updateCurrentGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, modifierKey));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Hadamard Gate", KeyEvent.VK_M);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getCurrentGraphName(), QVertex.Type.HADAMARD);
				updateCurrentGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, modifierKey));
		graphAddMenu.add(item);
		
		graphMenu.add(graphAddMenu);
		
		if (!isMac) {
			item = new JMenuItem("Quit", KeyEvent.VK_Q);
			// TODO: make it work
			graphMenu.add(item);
		}
		
		item = new JMenuItem("Show/Hide Console", KeyEvent.VK_C);
		item.addActionListener(new QuantoFrameListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				showHideConsole();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_BACK_QUOTE, modifierKey));
		viewMenu.add(item);
		mb.add(graphMenu);
		mb.add(viewMenu);
		
		
		setJMenuBar(mb);
		
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(tabs, BorderLayout.CENTER);
		
		Map<String,QuantoVisualizer> views = new HashMap<String, QuantoVisualizer>();
        
        console = new QuantoConsole(tabs, views);
        core = console.qcore;
        getContentPane().add(console, BorderLayout.NORTH);
        
        try {
        	newGraph();
        } catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
        
        this.pack();
        
        showHideConsole();
	}
	
	public void newGraph() throws QuantoCore.ConsoleError {
		String g = core.new_graph();
		console.updateGraph(g);
	}
	
	public String getCurrentGraphName() {
		return tabs.getTitleAt(tabs.getSelectedIndex());
	}
	
	public void updateCurrentGraph() throws QuantoCore.ConsoleError {
		console.updateGraph(getCurrentGraphName());
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
