package quanto.gui;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.LayoutDecorator;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.apache.commons.collections15.Transformer;
import quanto.gui.QuantoCore.ConsoleError;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.contrib.ConstrainedPickingGraphMousePlugin;
import edu.uci.ics.jung.contrib.ViewScrollingGraphMousePlugin;
import edu.uci.ics.jung.contrib.ViewZoomScrollPane;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;

public class InteractiveGraphView
	extends InteractiveView
	implements AddEdgeGraphMousePlugin.Adder<QVertex>,
	           KeyListener {

	private static final long serialVersionUID = 7196565776978339937L;

	public Map<String, ActionListener> actionMap = new HashMap<String, ActionListener>();
	public static final String SAVE_GRAPH_ACTION = "save-command";
	public static final String SAVE_GRAPH_AS_ACTION = "save-as-command";
	public static final String ABORT_ACTION = "abort-command";
	public static final String EXPORT_TO_PDF_ACTION = "export-to-pdf-command";
	public static final String SELECT_MODE_ACTION = "select-mode-command";
	public static final String EDGE_MODE_ACTION = "edge-mode-command";
	public static final String LATEX_TO_CLIPBOARD_ACTION = "latex-to-clipboard-command";
	public static final String ADD_RED_VERTEX_ACTION = "add-red-vertex-command";
	public static final String ADD_GREEN_VERTEX_ACTION = "add-green-vertex-command";
	public static final String ADD_BOUNDARY_VERTEX_ACTION = "add-boundary-vertex-command";
	public static final String ADD_HADAMARD_ACTION = "add-hadamard-vertex-command";
	public static final String SHOW_REWRITES_ACTION = "show-rewrites-command";
	public static final String NORMALISE_ACTION = "normalise-command";
	public static final String FAST_NORMALISE_ACTION = "fast-normalise-command";
	public static final String LOCK_VERTICES_ACTION = "lock-vertices-command";
	public static final String UNLOCK_VERTICES_ACTION = "unlock-vertices-command";
	public static final String FLIP_VERTEX_COLOUR_ACTION = "flip-vertex-colour-command";
	public static final String BANG_VERTICES_ACTION = "bang-vertices-command";
	public static final String UNBANG_VERTICES_ACTION = "unbang-vertices-command";
	public static final String DROP_BANG_BOX_ACTION = "drop-bang-box-command";
	public static final String KILL_BANG_BOX_ACTION = "kill-bang-box-command";
	public static final String DUPLICATE_BANG_BOX_ACTION = "duplicate-bang-box-command";
	public static final String DUMP_HILBERT_TERM_AS_TEXT = "hilbert-as-text-command";
	public static final String DUMP_HILBERT_TERM_AS_MATHEMATICA = "hilbert-as-mathematica-command";

	private GraphVisualizationViewer viewer;
	private QuantoCore core;
	private RWMouse graphMouse;
	private volatile Thread rewriter = null;
	private List<Rewrite> rewriteCache = null;

	public boolean viewHasParent() {
		return this.getParent() != null;
	}

	private class QVertexLabeler implements VertexLabelRenderer {

		Map<QVertex, Labeler> components;

		public QVertexLabeler() {
			components = new HashMap<QVertex, Labeler>();
		}

		public <T> Component getVertexLabelRendererComponent(JComponent vv,
								     Object value, Font font, boolean isSelected, T vertex) {
			if (vertex instanceof QVertex && ((QVertex) vertex).isAngleVertex()) {
				final QVertex qVertex = (QVertex) vertex;
				Point2D screen = viewer.getRenderContext().
					getMultiLayerTransformer().transform(
					viewer.getGraphLayout().transform(qVertex));

				// lazily create the labeler
				Labeler angleLabeler = components.get(qVertex);
				if (angleLabeler == null) {
					angleLabeler = new Labeler("");
					components.put(qVertex, angleLabeler);
					viewer.add(angleLabeler);
					if (qVertex.getColor().equals(Color.red)) {
						angleLabeler.setColor(new Color(255, 170, 170));
					}
					else {
						angleLabeler.setColor(new Color(150, 255, 150));
					}

					String angle = ((QVertex) vertex).getAngle();
					Rectangle rect = new Rectangle(angleLabeler.getPreferredSize());
					Point loc = new Point((int) (screen.getX() - rect.getCenterX()),
							      (int) screen.getY() + 10);
					rect.setLocation(loc);

					if (!angleLabeler.getText().equals(angle)) {
						angleLabeler.setText(angle);
					}
					if (!angleLabeler.getBounds().equals(rect)) {
						angleLabeler.setBounds(rect);
					}

					angleLabeler.addChangeListener(new ChangeListener() {

						public void stateChanged(ChangeEvent e) {
							Labeler lab = (Labeler) e.getSource();
							if (qVertex != null) {
								try {
									getCore().set_angle(getGraph(),
											    qVertex, lab.getText());
									updateGraph();
								}
								catch (QuantoCore.ConsoleError err) {
									errorDialog(err.getMessage());
								}
							}
						}
					});
				}
				String angle = qVertex.getAngle();
				Rectangle rect = new Rectangle(angleLabeler.getPreferredSize());
				Point loc = new Point((int) (screen.getX() - rect.getCenterX()),
						      (int) screen.getY() + 10);
				rect.setLocation(loc);

				if (!angleLabeler.getText().equals(angle)) {
					angleLabeler.setText(angle);
				}
				if (!angleLabeler.getBounds().equals(rect)) {
					angleLabeler.setBounds(rect);
				}

				return new JLabel();
			}
			else if (!(vertex instanceof QVertex)
				|| ((QVertex) vertex).getVertexType() != QVertex.Type.BOUNDARY) {
				JLabel label = new JLabel((String) value);
				label.setOpaque(true);
				label.setBackground(Color.white);
				return label;
			}
			return new JLabel();
		}

		/**
		 * Removes orphaned labels.
		 */
		public void cleanup() {
			final Map<QVertex, Labeler> oldComponents = components;
			components = new HashMap<QVertex, Labeler>();
			for (Labeler l : oldComponents.values()) {
				viewer.remove(l);
			}
		}
	}

	/**
	 * A graph mouse for doing most interactive graph operations.
	 *
	 */
	private class RWMouse extends PluggableGraphMouse {

		private GraphMousePlugin pickingMouse, edgeMouse;
		private boolean pickingMouseActive, edgeMouseActive;

		public RWMouse() {
			int mask = InputEvent.CTRL_MASK;
			if (QuantoApp.isMac) {
				mask = InputEvent.META_MASK;
			}

			add(new ScalingGraphMousePlugin(new ViewScalingControl(), mask));
			add(new ViewTranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | mask));
			ViewScrollingGraphMousePlugin scrollerPlugin = new ViewScrollingGraphMousePlugin();
			scrollerPlugin.setShift(10.0);
			add(scrollerPlugin);
			add(new AddEdgeGraphMousePlugin<QVertex, QEdge>(
				viewer,
				InteractiveGraphView.this,
				InputEvent.BUTTON1_MASK | InputEvent.ALT_MASK));
			pickingMouse = new BangBoxAwarePickerMousePlugin();
			edgeMouse = new AddEdgeGraphMousePlugin<QVertex, QEdge>(
				viewer,
				InteractiveGraphView.this,
				InputEvent.BUTTON1_MASK);
			setPickingMouse();
		}

		public void clearMouse() {
			edgeMouseActive = false;
			remove(edgeMouse);

			pickingMouseActive = false;
			remove(pickingMouse);
		}

		public void setPickingMouse() {
			clearMouse();
			pickingMouseActive = true;
			add(pickingMouse);
			InteractiveGraphView.this.repaint();
			if (isAttached()) {
				getViewPort().setCommandStateSelected(SELECT_MODE_ACTION, true);
			}
		}

		public void setEdgeMouse() {
			clearMouse();
			edgeMouseActive = true;
			add(edgeMouse);
			InteractiveGraphView.this.repaint();
			if (isAttached()) {
				getViewPort().setCommandStateSelected(EDGE_MODE_ACTION, true);
			}
		}

		public boolean isPickingMouse() {
			return pickingMouseActive;
		}

		public boolean isEdgeMouse() {
			return edgeMouseActive;
		}
	}

	public InteractiveGraphView(QuantoCore core, QuantoGraph g) {
		this(core, g, new Dimension(800, 600));
	}

	public InteractiveGraphView(QuantoCore core, QuantoGraph g, Dimension size) {
		super(new BorderLayout(), g.getName());
		setPreferredSize(size);

		viewer = new GraphVisualizationViewer(g);
		add(new ViewZoomScrollPane(viewer), BorderLayout.CENTER);

		this.core = core;
		viewer.setLayoutSmoothingEnabled(true);

		Relaxer r = viewer.getModel().getRelaxer();
		if (r != null) {
			r.setSleepTime(10);
		}

		graphMouse = new RWMouse();
		viewer.setGraphMouse(graphMouse);

		viewer.addPreRenderPaintable(new VisualizationServer.Paintable() {

			public void paint(Graphics g) {
				Color old = g.getColor();
				g.setColor(Color.red);
				if (graphMouse.isEdgeMouse()) {
					g.drawString("EDGE MODE", 5, 15);
				}
				g.setColor(old);
			}

			public boolean useTransform() {
				return false;
			}
		});

		viewer.addMouseListener(new MouseAdapter() {

			@Override
			public void mousePressed(MouseEvent e) {
				InteractiveGraphView.this.grabFocus();
				super.mousePressed(e);
			}
		});

		addKeyListener(this);
		viewer.addKeyListener(this);

		viewer.getRenderContext().setVertexStrokeTransformer(
			new Transformer<QVertex, Stroke>() {

				public Stroke transform(QVertex v) {
					if (viewer.getPickedVertexState().isPicked(v)
						|| viewer.isLocked(v)) {
						return new BasicStroke(2);
					}
					else {
						return new BasicStroke(1);
					}
				}
			});
		viewer.getRenderContext().setVertexDrawPaintTransformer(
			new Transformer<QVertex, Paint>() {

				public Paint transform(QVertex v) {
					if (viewer.getPickedVertexState().isPicked(v)) {
						return Color.blue;
					}
					else if (viewer.isLocked(v)) {
						return Color.gray;
					}
					else {
						return Color.black;
					}
				}
			});


		viewer.getRenderContext().setVertexLabelRenderer(new QVertexLabeler());

		viewer.setBoundingBoxEnabled(true);

		buildActionMap();
	}

	public GraphVisualizationViewer getVisualization() {
		return viewer;
	}

	public void addChangeListener(ChangeListener listener) {
		viewer.addChangeListener(listener);
	}

	public QuantoGraph getGraph() {
		return viewer.getGraph();
	}

	/**
	 * Compute a bounding box and scale such that the largest
	 * dimension fits within the view port.
	 */
	public void zoomToFit() {
		viewer.zoomToFit(getSize());
	}

	public static String titleOfGraph(String name) {
		return "graph (" + name + ")";
	}

	public void addEdge(QVertex s, QVertex t) {
		try {
			getCore().add_edge(getGraph(), s, t);
			updateGraph();
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}

	public void addVertex(QVertex.Type type) {
		try {
			getCore().add_vertex(getGraph(), type);
			updateGraph();
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}

	public void showRewrites() {
		try {
			Set<QVertex> picked = viewer.getPickedVertexState().getPicked();
			if (picked.isEmpty()) {
				getCore().attach_rewrites(getGraph(), getGraph().getVertices());
			}
			else {
				getCore().attach_rewrites(getGraph(), picked);
			}
			JFrame rewrites = new RewriteViewer(InteractiveGraphView.this);
			rewrites.setVisible(true);
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}

	public void updateGraph() throws QuantoCore.ConsoleError {
		String xml = getCore().graph_xml(getGraph());
		try {
			getGraph().fromXml(xml);
		}
		catch (QuantoGraph.ParseException e) {
			throw new ConsoleError("The core sent an invalid graph description: " + e.getMessage());
		}
		viewer.relayout();

		// clean up un-needed labels:
		((QVertexLabeler) viewer.getRenderContext().getVertexLabelRenderer()).cleanup();

		// re-validate the picked state
		QVertex[] oldPicked =
			viewer.getPickedVertexState().getPicked().toArray(
			new QVertex[viewer.getPickedVertexState().getPicked().size()]);
		viewer.getPickedVertexState().clear();
		Map<String, QVertex> vm = getGraph().getVertexMap();
		for (QVertex v : oldPicked) {
			QVertex new_v = vm.get(v.getName());
			if (new_v != null) {
				viewer.getPickedVertexState().pick(new_v, true);
			}
		}

		if (isAttached()) {
			getViewPort().setCommandEnabled(SAVE_GRAPH_ACTION,
				!getGraph().isSaved()
				);
		}

		viewer.update();
	}

	public void outputToTextView(String text) {
		TextView tview = new TextView(getTitle() + "-output", text);
		getViewManager().addView(tview);

		if (isAttached())
			getViewPort().openView(tview);
	}
	private SubgraphHighlighter highlighter = null;

	public void clearHighlight() {
		if (highlighter != null) {
			viewer.removePostRenderPaintable(highlighter);
		}
		highlighter = null;
		viewer.repaint();
	}

	public void highlightSubgraph(QuantoGraph g) {
		clearHighlight();
		highlighter = new SubgraphHighlighter(g);
		viewer.addPostRenderPaintable(highlighter);
		viewer.update();
	}

	public void startRewriting() {
		abortRewriting();
		rewriter = new RewriterThread();
		rewriter.start();
		if (isAttached()) {
			setupNormaliseAction(getViewPort());
		}
	}

	public void abortRewriting() {
		if (rewriter != null) {
			rewriter.interrupt();
			rewriter = null;
		}
		if (isAttached()) {
			setupNormaliseAction(getViewPort());
		}
	}

	private void setupNormaliseAction(ViewPort vp) {
		if (rewriter == null) {
			vp.setCommandEnabled(ABORT_ACTION, false);
			vp.setCommandEnabled(NORMALISE_ACTION, true);
		}
		else {
			vp.setCommandEnabled(ABORT_ACTION, true);
			vp.setCommandEnabled(NORMALISE_ACTION, false);
		}
	}

	private class RewriterThread extends Thread {

		private boolean highlight = false;

		private void attachNextRewrite() {
			try {
				getCore().attach_one_rewrite(
					getGraph(),
					getGraph().getVertices());
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
		}

		private void invokeHighlightSubgraphAndWait(QuantoGraph subgraph)
			throws InterruptedException {
			highlight = true;
			final QuantoGraph fSubGraph = subgraph;
			invokeAndWait(new Runnable() {

				public void run() {
					highlightSubgraph(fSubGraph);
				}
			});
		}

		private void invokeApplyRewriteAndWait(int index)
			throws InterruptedException {
			highlight = false;
			final int fIndex = index;
			invokeAndWait(new Runnable() {

				public void run() {
					clearHighlight();
					applyRewrite(fIndex);
				}
			});
		}

		private void invokeClearHighlightLater() {
			highlight = false;
			SwingUtilities.invokeLater(new Runnable() {

				public void run() {
					clearHighlight();
				}
			});
		}

		private void invokeInfoDialogAndWait(String message)
			throws InterruptedException {
			final String fMessage = message;
			invokeAndWait(new Runnable() {

				public void run() {
					infoDialog(fMessage);
				}
			});
		}

		private void invokeAndWait(Runnable runnable)
			throws InterruptedException {
			try {
				SwingUtilities.invokeAndWait(runnable);
			}
			catch (InvocationTargetException ex) {
				ex.printStackTrace();
			}
		}

		@Override
		public void run() {
			try {
				attachNextRewrite();
				List<Rewrite> rws = getRewrites();
				int count = 0;
				Random r = new Random();
				int rw = 0;
				while (rws.size() > 0
					&& !Thread.interrupted()) {
					rw = r.nextInt(rws.size());
					invokeHighlightSubgraphAndWait(rws.get(rw).getLhs());
					sleep(1500);
					invokeApplyRewriteAndWait(rw);
					++count;
					attachNextRewrite();
					rws = getRewrites();
				}

				invokeInfoDialogAndWait("Applied " + count + " rewrites");
			}
			catch (InterruptedException e) {
				if (highlight) {
					invokeClearHighlightLater();
				}
			}
		}
	}

	private class SubgraphHighlighter
		implements VisualizationServer.Paintable {

		Collection<QVertex> verts;

		public SubgraphHighlighter(QuantoGraph g) {
			verts = getGraph().getSubgraphVertices(g);
		}

		public void paint(Graphics g) {
			Color oldColor = g.getColor();
			g.setColor(Color.blue);
			Graphics2D g2 = (Graphics2D) g.create();
			float opac = 0.3f + 0.2f * (float) Math.sin(
				System.currentTimeMillis() / 150.0);
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opac));

			for (QVertex v : verts) {
				Point2D pt = viewer.getGraphLayout().transform(v);
				Ellipse2D ell = new Ellipse2D.Double(
					pt.getX() - 15, pt.getY() - 15, 30, 30);
				Shape draw = viewer.getRenderContext().getMultiLayerTransformer().transform(ell);
				((Graphics2D) g2).fill(draw);
			}

			g2.dispose();
			g.setColor(oldColor);
			repaint(10);
		}

		public boolean useTransform() {
			return false;
		}
	}

	/**
	 * Gets the attached rewrites as a list of Pair<QuantoGraph>. Returns and empty
	 * list on console error.
	 * @return
	 */
	public List<Rewrite> getRewrites() {
		try {
			String xml = getCore().show_rewrites(getGraph());
			rewriteCache = Rewrite.parseRewrites(xml);
			return rewriteCache;
		}
		catch (QuantoGraph.ParseException e) {
			errorDialog("The core sent an invalid graph description: " + e.getMessage());
		}
		catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}

		return new ArrayList<Rewrite>();
	}

	public void applyRewrite(int index) {
		try {
			if (rewriteCache != null && rewriteCache.size() > index) {
				List<QVertex> sub = getGraph().getSubgraphVertices(
					rewriteCache.get(index).getLhs());
				if (sub.size() > 0) {
					Rectangle2D rect = viewer.getSubgraphBounds(sub);
					viewer.setSmoothingOrigin(rect.getCenterX(), rect.getCenterY());
				}
			}
			getCore().apply_rewrite(getGraph(), index);
			updateGraph();
		}
		catch (ConsoleError e) {
			errorDialog("Error in rewrite. The graph probably changed "
				+ "after this rewrite was attached.");
		}
	}

	private QuantoCore getCore() {
		return core;
	}

	public void commandTriggered(String command) {
		ActionListener listener = actionMap.get(command);
		if (listener != null)
			listener.actionPerformed(new ActionEvent(this, -1, command));
	}

	public void saveGraphAs() {
		int retVal = QuantoApp.getInstance().getFileChooser().showSaveDialog(this);
		if (retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File f = QuantoApp.getInstance().getFileChooser().getSelectedFile();
				String filename = f.getCanonicalPath().replaceAll("\\n|\\r", "");
				core.save_graph(getGraph(), filename);
				getGraph().setFileName(filename);
				getGraph().setSaved(true);
				setTitle(f.getName());
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
			catch (java.io.IOException ioe) {
				errorDialog(ioe.getMessage());
			}
		}
	}

	public void saveGraph() {
		if (getGraph().getFileName() != null) {
			try {
				getCore().save_graph(getGraph(), getGraph().getFileName());
				getGraph().setSaved(true);
			}
			catch (QuantoCore.ConsoleError e) {
				errorDialog(e.getMessage());
			}
		}
		else {
			saveGraphAs();
		}
	}

	public static void registerKnownCommands() {
		ViewPort.registerCommand(SAVE_GRAPH_ACTION);
		ViewPort.registerCommand(SAVE_GRAPH_AS_ACTION);
		ViewPort.registerCommand(ABORT_ACTION);
		ViewPort.registerCommand(EXPORT_TO_PDF_ACTION);
		ViewPort.registerCommand(SELECT_MODE_ACTION);
		ViewPort.registerCommand(EDGE_MODE_ACTION);
		ViewPort.registerCommand(LATEX_TO_CLIPBOARD_ACTION);
		ViewPort.registerCommand(ADD_RED_VERTEX_ACTION);
		ViewPort.registerCommand(ADD_GREEN_VERTEX_ACTION);
		ViewPort.registerCommand(ADD_BOUNDARY_VERTEX_ACTION);
		ViewPort.registerCommand(ADD_HADAMARD_ACTION);
		ViewPort.registerCommand(SHOW_REWRITES_ACTION);
		ViewPort.registerCommand(NORMALISE_ACTION);
		ViewPort.registerCommand(FAST_NORMALISE_ACTION);
		ViewPort.registerCommand(LOCK_VERTICES_ACTION);
		ViewPort.registerCommand(UNLOCK_VERTICES_ACTION);
		ViewPort.registerCommand(FLIP_VERTEX_COLOUR_ACTION);
		ViewPort.registerCommand(BANG_VERTICES_ACTION);
		ViewPort.registerCommand(UNBANG_VERTICES_ACTION);
		ViewPort.registerCommand(DROP_BANG_BOX_ACTION);
		ViewPort.registerCommand(KILL_BANG_BOX_ACTION);
		ViewPort.registerCommand(DUPLICATE_BANG_BOX_ACTION);
		ViewPort.registerCommand(DUMP_HILBERT_TERM_AS_TEXT);
		ViewPort.registerCommand(DUMP_HILBERT_TERM_AS_MATHEMATICA);
	}

	private void buildActionMap() {
		actionMap.put(SAVE_GRAPH_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveGraph();
			}
		});
		actionMap.put(SAVE_GRAPH_AS_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveGraphAs();
			}
		});

		actionMap.put(ViewPort.UNDO_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().undo(getGraph());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(ViewPort.REDO_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().redo(getGraph());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(ViewPort.CUT_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Set<QVertex> picked = viewer.getPickedVertexState().getPicked();
					if (!picked.isEmpty()) {
							getCore().copy_subgraph(getGraph(), "__clip__", picked);
							getCore().delete_vertices(getGraph(), picked);
							updateGraph();
					}
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(ViewPort.COPY_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Set<QVertex> picked = viewer.getPickedVertexState().getPicked();
					if (!picked.isEmpty()) {
							getCore().copy_subgraph(getGraph(), "__clip__", picked);
					}
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(ViewPort.PASTE_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().insert_graph(getGraph(), "__clip__");
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(ViewPort.SELECT_ALL_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				synchronized (getGraph()) {
					for (QVertex v : getGraph().getVertices()) {
						viewer.getPickedVertexState().pick(v, true);
					}
				}
			}
		});
		actionMap.put(ViewPort.DESELECT_ALL_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				viewer.getPickedVertexState().clear();
			}
		});

		actionMap.put(EXPORT_TO_PDF_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					GraphVisualizationViewer view = new GraphVisualizationViewer(getGraph());
					byte[] gr = view.exportPdf();

					JFileChooser chooser = QuantoApp.getInstance().getFileChooser();
					int retVal = chooser.showSaveDialog(InteractiveGraphView.this);
					if (retVal == JFileChooser.APPROVE_OPTION) {
						File outputFile = chooser.getSelectedFile();
						if (outputFile.exists()) {
							int overwriteAnswer = JOptionPane.showConfirmDialog(
								InteractiveGraphView.this,
								"Are you sure you want to overwrite \"" + outputFile.getName() + "\"?",
								"Overwrite file?",
								JOptionPane.YES_NO_OPTION);
							if (overwriteAnswer != JOptionPane.YES_OPTION)
								return;
						}
						BufferedOutputStream file = new BufferedOutputStream(
							new FileOutputStream(outputFile));
						file.write(gr);
						file.close();
					}
				}
				catch (IOException ex) {
					errorDialog("Error writing file", ex.getMessage());
				}
			}
		});
		actionMap.put(SELECT_MODE_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				graphMouse.setPickingMouse();
			}
		});
		actionMap.put(EDGE_MODE_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				graphMouse.setEdgeMouse();
			}
		});
		actionMap.put(LATEX_TO_CLIPBOARD_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String tikz = TikzOutput.generate(getGraph(), viewer.getGraphLayout());
				Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
				StringSelection data = new StringSelection(tikz);
				cb.setContents(data, data);
			}
		});
		actionMap.put(ADD_RED_VERTEX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addVertex(QVertex.Type.RED);
			}
		});
		actionMap.put(ADD_GREEN_VERTEX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addVertex(QVertex.Type.GREEN);
			}
		});
		actionMap.put(ADD_BOUNDARY_VERTEX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addVertex(QVertex.Type.BOUNDARY);
			}
		});
		actionMap.put(ADD_HADAMARD_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addVertex(QVertex.Type.HADAMARD);
			}
		});
		actionMap.put(SHOW_REWRITES_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showRewrites();
			}
		});
		actionMap.put(NORMALISE_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (rewriter != null)
					abortRewriting();
				startRewriting();
			}
		});
		actionMap.put(ABORT_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (rewriter != null)
					abortRewriting();
			}
		});
		actionMap.put(FAST_NORMALISE_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().fastNormalise(getGraph());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(LOCK_VERTICES_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				viewer.lock(viewer.getPickedVertexState().getPicked());
				repaint();
			}
		});
		actionMap.put(UNLOCK_VERTICES_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				viewer.unlock(viewer.getPickedVertexState().getPicked());
				repaint();
			}
		});
		actionMap.put(FLIP_VERTEX_COLOUR_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().flip_vertices(getGraph(), viewer.getPickedVertexState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(BANG_VERTICES_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					// this is not the real bang box, but we just need the name
					BangBox bb = new BangBox(getCore().add_bang(getGraph()));
					getCore().bang_vertices(getGraph(), bb, viewer.getPickedVertexState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(UNBANG_VERTICES_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().unbang_vertices(getGraph(), viewer.getPickedVertexState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(DROP_BANG_BOX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().bbox_drop(getGraph(), viewer.getPickedBangBoxState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(KILL_BANG_BOX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().bbox_kill(getGraph(), viewer.getPickedBangBoxState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(DUPLICATE_BANG_BOX_ACTION, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					getCore().bbox_duplicate(getGraph(), viewer.getPickedBangBoxState().getPicked());
					updateGraph();
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});

		actionMap.put(DUMP_HILBERT_TERM_AS_TEXT, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					outputToTextView(getCore().hilb(getGraph(), "plain"));
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
		actionMap.put(DUMP_HILBERT_TERM_AS_MATHEMATICA, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					outputToTextView(getCore().hilb(getGraph(), "mathematica"));
				}
				catch (ConsoleError ex) {
					errorDialog("Console Error", ex.getMessage());
				}
			}
		});
	}

	public void attached(ViewPort vp) {
		for (String actionName : actionMap.keySet()) {
			vp.setCommandEnabled(actionName, true);
		}
		vp.setCommandEnabled(SAVE_GRAPH_ACTION,
			!getGraph().isSaved()
			);
		if (graphMouse.isEdgeMouse())
			vp.setCommandStateSelected(EDGE_MODE_ACTION, true);
		else
			vp.setCommandStateSelected(SELECT_MODE_ACTION, true);
		setupNormaliseAction(vp);
	}

	public void detached(ViewPort vp) {
		vp.setCommandStateSelected(SELECT_MODE_ACTION, true);

		for (String actionName : actionMap.keySet()) {
			vp.setCommandEnabled(actionName, false);
		}
	}

	public void cleanUp() {
	}

	@Override
	protected String getUnsavedClosingMessage() {
		return "Graph '" + getGraph().getName() + "' is unsaved. Close anyway?";
	}

	public boolean isSaved() {
		return getGraph().isSaved();
	}

	static class BangBoxAwarePickerMousePlugin
		extends ConstrainedPickingGraphMousePlugin<QVertex, QEdge> {

		protected BangBox bangBox;

		public BangBoxAwarePickerMousePlugin() {
			super(ConstrainingAction.MoveOthers, 20, 20);
		}

		private BangBox getBangBox(Layout<QVertex, QEdge> layout, double x, double y) {
			while (layout instanceof LayoutDecorator) {
				layout = ((LayoutDecorator<QVertex, QEdge>) layout).getDelegate();
			}
			try {
				LockableBangBoxLayout<QVertex, QEdge> realLayout = (LockableBangBoxLayout<QVertex, QEdge>) layout;
				QuantoGraph graph = (QuantoGraph) realLayout.getGraph();
				synchronized (graph) {
					for (BangBox bb : graph.getBangBoxes()) {
						Rectangle2D bbRect = realLayout.transformBangBox(bb);
						if (bbRect == null) {
							System.err.println("Layout hasn't caught up with us yet");
						}
						else if (bbRect.contains(x, y)) {
							return bb;
						}
					}
				}
			}
			catch (ClassCastException ex) {
				System.err.println("When finding bang box: " + ex.getMessage());
			}
			return null;
		}

		@Override
		@SuppressWarnings("unchecked")
		public void mousePressed(MouseEvent e) {
			down = e.getPoint();
			VisualizationViewer<QVertex, QEdge> vv = (VisualizationViewer) e.getSource();
			GraphElementAccessor<QVertex, QEdge> pickSupport = vv.getPickSupport();
			PickedState<QVertex> pickedVertexState = vv.getPickedVertexState();
			PickedState<QEdge> pickedEdgeState = vv.getPickedEdgeState();
			PickedState<BangBox> pickedBangBoxState = ((GraphVisualizationViewer) vv).getPickedBangBoxState();
			if (pickSupport != null && pickedVertexState != null) {
				Layout<QVertex, QEdge> layout = vv.getGraphLayout();
				if (e.getModifiers() == modifiers) {
					rect.setFrameFromDiagonal(down, down);
					// p is the screen point for the mouse event
					Point2D ip = e.getPoint();

					vertex = pickSupport.getVertex(layout, ip.getX(), ip.getY());
					if (vertex != null) {
						if (pickedVertexState.isPicked(vertex) == false) {
							pickedVertexState.clear();
							pickedEdgeState.clear();
							pickedBangBoxState.clear();
							pickedVertexState.pick(vertex, true);
						}
						// layout.getLocation applies the layout transformer so
						// q is transformed by the layout transformer only
						Point2D q = layout.transform(vertex);
						// transform the mouse point to graph coordinate system
						Point2D gp = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(Layer.LAYOUT, ip);

						offsetx = (float) (gp.getX() - q.getX());
						offsety = (float) (gp.getY() - q.getY());
					}
					else if ((edge = pickSupport.getEdge(layout, ip.getX(), ip.getY())) != null) {
						pickedEdgeState.clear();
						pickedVertexState.clear();
						pickedBangBoxState.clear();
						pickedEdgeState.pick(edge, true);
					}
					else if ((bangBox = getBangBox(layout, ip.getX(), ip.getY())) != null) {
						pickedEdgeState.clear();
						pickedVertexState.clear();
						pickedBangBoxState.clear();
						pickedBangBoxState.pick(bangBox, true);
					}
					else {
						vv.addPostRenderPaintable(lensPaintable);
						pickedEdgeState.clear();
						pickedVertexState.clear();
						pickedBangBoxState.clear();
					}

				}
				else if (e.getModifiers() == addToSelectionModifiers) {
					vv.addPostRenderPaintable(lensPaintable);
					rect.setFrameFromDiagonal(down, down);
					Point2D ip = e.getPoint();
					vertex = pickSupport.getVertex(layout, ip.getX(), ip.getY());
					if (vertex != null) {
						boolean wasThere = pickedVertexState.pick(vertex, !pickedVertexState.isPicked(vertex));
						if (wasThere) {
							vertex = null;
						}
						else {

							// layout.getLocation applies the layout transformer so
							// q is transformed by the layout transformer only
							Point2D q = layout.transform(vertex);
							// translate mouse point to graph coord system
							Point2D gp = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(Layer.LAYOUT, ip);

							offsetx = (float) (gp.getX() - q.getX());
							offsety = (float) (gp.getY() - q.getY());
						}
					}
					else if ((edge = pickSupport.getEdge(layout, ip.getX(), ip.getY())) != null) {
						pickedEdgeState.pick(edge, !pickedEdgeState.isPicked(edge));
					}
					else if ((bangBox = getBangBox(layout, ip.getX(), ip.getY())) != null) {
						pickedBangBoxState.pick(bangBox, !pickedBangBoxState.isPicked(bangBox));
					}
				}
			}
			if (vertex != null) {
				e.consume();
			}

		}

		@Override
		@SuppressWarnings("unchecked")
		public void mouseReleased(MouseEvent e) {
			boolean recalcSize = false;
			if (vertex != null) {
				recalcSize = true;
			}

			super.mouseReleased(e);
			bangBox = null;

			if (recalcSize) {
				VisualizationViewer<QVertex, QEdge> vv = (VisualizationViewer) e.getSource();
				Layout<QVertex, QEdge> layout = vv.getGraphLayout();
				while (layout instanceof LayoutDecorator) {
					layout = ((LayoutDecorator<QVertex, QEdge>) layout).getDelegate();
				}
				try {
					LockableBangBoxLayout<QVertex, QEdge> realLayout = (LockableBangBoxLayout<QVertex, QEdge>) layout;
					realLayout.recalculateBounds();
				}
				catch (ClassCastException ex) {
					System.err.println("When mouse released: " + ex.getMessage());
				}
			}
		}

		@Override
		public void mouseEntered(MouseEvent e) {
			// don't change the cursor
		}

		@Override
		public void mouseExited(MouseEvent e) {
			// don't change the cursor
		}
	}

	public void keyPressed(KeyEvent e) {
		// this listener only handles un-modified keys
		if (e.getModifiers() != 0) {
			return;
		}

		int delete = (QuantoApp.isMac) ? KeyEvent.VK_BACK_SPACE : KeyEvent.VK_DELETE;
		if (e.getKeyCode() == delete) {
			try {
				getCore().delete_edges(
					getGraph(), viewer.getPickedEdgeState().getPicked());
				getCore().delete_vertices(
					getGraph(), viewer.getPickedVertexState().getPicked());
				updateGraph();

			}
			catch (QuantoCore.ConsoleError err) {
				errorDialog(err.getMessage());
			}
			finally {
				// if null things are in the picked state, weird stuff
				// could happen.
				viewer.getPickedEdgeState().clear();
				viewer.getPickedVertexState().clear();
			}
		}
		else {
			switch (e.getKeyCode()) {
				case KeyEvent.VK_R:
					addVertex(QVertex.Type.RED);
					break;
				case KeyEvent.VK_G:
					addVertex(QVertex.Type.GREEN);
					break;
				case KeyEvent.VK_H:
					addVertex(QVertex.Type.HADAMARD);
					break;
				case KeyEvent.VK_B:
					addVertex(QVertex.Type.BOUNDARY);
					break;
				case KeyEvent.VK_E:
					if (graphMouse.isEdgeMouse()) {
						graphMouse.setPickingMouse();
					}
					else {
						graphMouse.setEdgeMouse();
					}
					break;
				case KeyEvent.VK_SPACE:
					showRewrites();
					break;
			}
		}
	}

	public void keyReleased(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	public void refresh() {
		try {
			updateGraph();
		}
		catch (ConsoleError ex) {
			errorDialog("Console erro", ex.getMessage());
		}
	}
}
