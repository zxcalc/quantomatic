package quanto.gui;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;

import java.io.File;
import javax.swing.*;
import quanto.gui.ViewPort.CommandAction;

public class QuantoFrame extends JFrame implements ViewPortHost {

	private static final long serialVersionUID = 3656684775223085393L;
	private QuantoCore core;
	private final ViewPort viewPort;
	private volatile static int frameCount = 0;
	private QuantoMenuBar menuBar = new QuantoMenuBar();
	private QuantoApp app;
	private TheoryTree theoryTree;
	private Action newGraphAction;
	private Action openGraphAction;
	private Action closeAction;

	public Action getNewGraphAction() {
		if (newGraphAction == null) {
			newGraphAction = new AbstractAction("New graph") {

				public void actionPerformed(ActionEvent e) {
					createNewGraph();
				}
			};
			newGraphAction.putValue(Action.ACCELERATOR_KEY,
					KeyStroke.getKeyStroke(
						KeyEvent.VK_N,
						QuantoApp.COMMAND_MASK));
			newGraphAction.putValue(Action.MNEMONIC_KEY, KeyEvent.VK_G);
			newGraphAction.putValue(Action.SHORT_DESCRIPTION, "Create a new empty graph");
		}
		return newGraphAction;
	}

	public Action getOpenGraphAction() {
		if (openGraphAction == null) {
			openGraphAction = new AbstractAction("Open graph...") {

				public void actionPerformed(ActionEvent e) {
					openGraph();
				}
			};
			openGraphAction.putValue(Action.ACCELERATOR_KEY,
					KeyStroke.getKeyStroke(
						KeyEvent.VK_O,
						QuantoApp.COMMAND_MASK));
			openGraphAction.putValue(Action.MNEMONIC_KEY, KeyEvent.VK_G);
			openGraphAction.putValue(Action.SHORT_DESCRIPTION, "Create a new empty graph");
		}
		return openGraphAction;
	}

	public Action getCloseAction() {
		if (closeAction == null) {
			closeAction = new AbstractAction("Close") {
				public void actionPerformed(ActionEvent e) {
					ViewPort.CloseResult result = viewPort.closeCurrentView();
					if (result == ViewPort.CloseResult.NoMoreViews)
						dispose();
				}
			};
			closeAction.putValue(Action.ACCELERATOR_KEY,
					KeyStroke.getKeyStroke(
						KeyEvent.VK_W,
						QuantoApp.COMMAND_MASK));
			closeAction.putValue(Action.MNEMONIC_KEY, KeyEvent.VK_C);
			closeAction.putValue(Action.SHORT_DESCRIPTION, "Close the current graph");
		}
		return closeAction;
	}

	public QuantoFrame(QuantoApp app) {
		super("Quantomatic");

		frameCount++;
		this.app = app;
		core = app.getCore();
		setBackground(Color.white);

		setJMenuBar(menuBar);

		viewPort = new ViewPort(app.getViewManager(), this);
		theoryTree = new TheoryTree(viewPort, core);

		getContentPane().setLayout(new BorderLayout());

		//Add the scroll panes to a split pane.
		JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setLeftComponent(theoryTree);
		splitPane.setRightComponent(viewPort);
		splitPane.setDividerLocation(150);

		getContentPane().add(splitPane, BorderLayout.CENTER);

		initMenuBar(menuBar);

		this.pack();
	}

	protected void initMenuBar(QuantoMenuBar menuBar) {
		createFileMenu(menuBar);
		createEditMenu(menuBar);
		createViewMenu(menuBar);
		createGraphMenu(menuBar);
		createHilbertMenu(menuBar);

		JMenu ruleMenu = menuBar.addMenu(QuantoMenuBar.RULE_MENU,
				"Rule",
				KeyEvent.VK_R);
		ruleMenu.add(viewPort.getCommandAction(SplitGraphView.USE_RULE_ACTION));
	}

	public void createFileMenu(QuantoMenuBar menu) {
		JMenu fileMenu = menuBar.addMenu(QuantoMenuBar.FILE_MENU,
				"File",
				KeyEvent.VK_F);

		fileMenu.add(app.getNewFrameAction());
		fileMenu.add(getNewGraphAction());
		fileMenu.add(getOpenGraphAction());
		fileMenu.add(viewPort.getCommandAction(InteractiveGraphView.SAVE_GRAPH_ACTION));
		fileMenu.add(viewPort.getCommandAction(InteractiveGraphView.SAVE_GRAPH_AS_ACTION));

		fileMenu.addSeparator();

		fileMenu.add(app.getLoadTheoryAction());
		fileMenu.add(app.getSaveTheoryAction());

		fileMenu.addSeparator();

		fileMenu.add(getCloseAction());

		if (!QuantoApp.MAC_OS_X) {
			fileMenu.add(app.getQuitAction());
		}
	}

	public void createEditMenu(QuantoMenuBar menu) {
		JMenu editMenu = menu.addMenu(QuantoMenuBar.EDIT_MENU,
				"Edit",
				KeyEvent.VK_E);

		editMenu.add(viewPort.getCommandAction(ViewPort.UNDO_ACTION));
		editMenu.add(viewPort.getCommandAction(ViewPort.REDO_ACTION));

		editMenu.addSeparator();

		editMenu.add(viewPort.getCommandAction(ViewPort.CUT_ACTION));
		editMenu.add(viewPort.getCommandAction(ViewPort.COPY_ACTION));
		editMenu.add(viewPort.getCommandAction(ViewPort.PASTE_ACTION));

		editMenu.addSeparator();

		editMenu.add(viewPort.getCommandAction(ViewPort.SELECT_ALL_ACTION));
		editMenu.add(viewPort.getCommandAction(ViewPort.DESELECT_ALL_ACTION));
	}

	private JMenuItem createBoolPrefMenuItem(QuantoApp.BoolPref pref) {
		JMenuItem item = new JCheckBoxMenuItem(pref.getFriendlyName());
		item.setSelected(
			app.getPreference(pref));
		item.addItemListener(pref);
		return item;
	}

	private void createViewMenu(QuantoMenuBar menuBar) {
		JMenu viewMenu = menuBar.addMenu(QuantoMenuBar.VIEW_MENU,
				"View",
				KeyEvent.VK_V);

		JMenuItem refreshAll = new JMenuItem("Refresh All Graphs", KeyEvent.VK_R);
		refreshAll.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				app.getViewManager().refreshAll();
			}
		});
		refreshAll.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_R,
			QuantoApp.COMMAND_MASK | Event.SHIFT_MASK));
		viewMenu.add(refreshAll);

		viewMenu.addSeparator();

		viewMenu.add(createBoolPrefMenuItem(QuantoApp.DRAW_ARROW_HEADS));
		viewMenu.add(createBoolPrefMenuItem(QuantoApp.CONSOLE_ECHO));
		viewMenu.add(createBoolPrefMenuItem(QuantoApp.SHOW_INTERNAL_NAMES));
		viewMenu.add(createBoolPrefMenuItem(QuantoApp.NEW_WINDOW_FOR_GRAPHS));
	}

	private void createGraphMenu(QuantoMenuBar menuBar) {
		JMenu graphMenu = menuBar.addMenu(QuantoMenuBar.GRAPH_MENU,
				"Graph",
				KeyEvent.VK_G);

		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.EXPORT_TO_PDF_ACTION));

		graphMenu.addSeparator();

		ButtonGroup mouseModeGroup = new ButtonGroup();
		JRadioButtonMenuItem rbPickingMode = new JRadioButtonMenuItem();
		CommandAction selectModeAction = viewPort.getCommandAction(
			InteractiveGraphView.SELECT_MODE_ACTION);
		rbPickingMode.setAction(selectModeAction);
		selectModeAction.associateButtonModel(rbPickingMode.getModel());
		mouseModeGroup.add(rbPickingMode);
		rbPickingMode.setSelected(true);
		graphMenu.add(rbPickingMode);

		JRadioButtonMenuItem rbEdgeMode = new JRadioButtonMenuItem();
		CommandAction edgeModeAction = viewPort.getCommandAction(
			InteractiveGraphView.EDGE_MODE_ACTION);
		rbEdgeMode.setAction(edgeModeAction);
		edgeModeAction.associateButtonModel(rbEdgeMode.getModel());
		mouseModeGroup.add(rbEdgeMode);
		rbEdgeMode.setSelected(false);
		graphMenu.add(rbEdgeMode);

		graphMenu.addSeparator();

		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.LATEX_TO_CLIPBOARD_ACTION));

		JMenu graphAddMenu = new JMenu("Add");
		graphMenu.add(graphAddMenu);
		graphAddMenu.add(viewPort.getCommandAction(InteractiveGraphView.ADD_RED_VERTEX_ACTION));
		graphAddMenu.add(viewPort.getCommandAction(InteractiveGraphView.ADD_GREEN_VERTEX_ACTION));
		graphAddMenu.add(viewPort.getCommandAction(InteractiveGraphView.ADD_BOUNDARY_VERTEX_ACTION));
		graphAddMenu.add(viewPort.getCommandAction(InteractiveGraphView.ADD_HADAMARD_ACTION));

		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.SHOW_REWRITES_ACTION));
		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.NORMALISE_ACTION));
		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.FAST_NORMALISE_ACTION));
		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.LOCK_VERTICES_ACTION));
		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.UNLOCK_VERTICES_ACTION));
		graphMenu.add(viewPort.getCommandAction(InteractiveGraphView.FLIP_VERTEX_COLOUR_ACTION));

		// define submenu for bang boxes
		JMenu bangMenu = new JMenu("Bang boxes");
		graphMenu.add(bangMenu);

		bangMenu.add(viewPort.getCommandAction(InteractiveGraphView.BANG_VERTICES_ACTION));
		bangMenu.add(viewPort.getCommandAction(InteractiveGraphView.UNBANG_VERTICES_ACTION));
		bangMenu.add(viewPort.getCommandAction(InteractiveGraphView.DROP_BANG_BOX_ACTION));
		bangMenu.add(viewPort.getCommandAction(InteractiveGraphView.KILL_BANG_BOX_ACTION));
		bangMenu.add(viewPort.getCommandAction(InteractiveGraphView.DUPLICATE_BANG_BOX_ACTION));
	}

	private void createHilbertMenu(QuantoMenuBar menuBar) {
		JMenu hilbMenu = menuBar.addMenu(QuantoMenuBar.HILBERT_MENU,
				"Hilbert space",
				KeyEvent.VK_B);

		hilbMenu.add(viewPort.getCommandAction(InteractiveGraphView.DUMP_HILBERT_TERM_AS_TEXT));
		hilbMenu.add(viewPort.getCommandAction(InteractiveGraphView.DUMP_HILBERT_TERM_AS_MATHEMATICA));
	}

	public void openView(InteractiveView view) {
		if (app.getPreference(QuantoApp.NEW_WINDOW_FOR_GRAPHS)) {
			app.openNewFrame(view);
		}
		else {
			viewPort.attachView(view);
		}
	}

	private void createNewGraph() {
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
		getCloseAction().setEnabled(allowed);
	}

	public boolean isViewAllowedToClose() {
		return getCloseAction().isEnabled();
	}
}
