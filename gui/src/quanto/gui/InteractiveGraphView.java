package quanto.gui;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.RenderContext;
import quanto.core.data.BangBox;
import quanto.core.data.Vertex;
import quanto.core.data.Edge;
import quanto.core.data.CoreGraph;
import quanto.core.data.VertexType;

import com.itextpdf.text.DocumentException;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.apache.commons.collections15.Transformer;

import quanto.core.CoreException;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.algorithms.layout.SmoothLayoutDecorator;
import edu.uci.ics.jung.contrib.visualization.control.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.contrib.visualization.control.ViewScrollingGraphMousePlugin;
import edu.uci.ics.jung.contrib.visualization.ViewZoomScrollPane;
import edu.uci.ics.jung.contrib.visualization.control.ConstrainedPickingBangBoxGraphMousePlugin;
import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxLabelRenderer;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.contrib.visualization.ShapeBangBoxPickSupport;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import java.awt.geom.AffineTransform;
import java.io.OutputStream;
import java.util.EventListener;
import java.util.EventObject;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.event.EventListenerList;
import quanto.core.data.AttachedRewrite;
import quanto.core.Core;
import quanto.core.protocol.userdata.CopyOfGraphUserDataSerializer;
import quanto.core.protocol.userdata.PositionGraphUserDataSerializer;
import quanto.gui.graphhelpers.ConstrainedMutableAffineTransformer;
import quanto.gui.graphhelpers.Labeler;
import quanto.gui.graphhelpers.QVertexRenderer;

public class InteractiveGraphView
		extends InteractiveView
		implements AddEdgeGraphMousePlugin.Adder<Vertex>,
		KeyListener {

	private static final long serialVersionUID = 7196565776978339937L;
	private static final Logger logger = Logger.getLogger("quanto.gui.InteractiveGraphView");
	public Map<String, ActionListener> actionMap = new HashMap<String, ActionListener>();
	private GraphVisualizationViewer viewer;
	private Core core;
	private RWMouse graphMouse;
	private volatile Job rewriter = null;
	private List<AttachedRewrite<CoreGraph>> rewriteCache = null;
	private JPanel indicatorPanel = null;
	private List<Job> activeJobs = null;
	private boolean saveEnabled = true;
	private boolean saveAsEnabled = true;
	private boolean directedEdges = false;
	private SmoothLayoutDecorator<Vertex, Edge> smoothLayout;
	private Map<String, Point2D> verticesCache;
	private QuantoForceLayout forceLayout;
	private QuantoDotLayout initLayout;

	public boolean viewHasParent() {
		return this.getParent() != null;
	}

	private class QVertexLabeler implements VertexLabelRenderer {

		Map<Vertex, Labeler> components;
		JLabel dummyLabel = new JLabel();
		JLabel realLabel = new JLabel();

		public QVertexLabeler() {
			components = new HashMap<Vertex, Labeler>();
			realLabel.setOpaque(true);
			realLabel.setBackground(Color.white);
		}

		public <T> Component getVertexLabelRendererComponent(JComponent vv,
				Object value, Font font, boolean isSelected, T vertex) {
			if (vertex instanceof Vertex) {
				final Vertex qVertex = (Vertex) vertex;
				if (!qVertex.isBoundaryVertex() && !qVertex.getVertexType().hasData()) {
					return dummyLabel;
				}

				Point2D screen = viewer.getRenderContext().
						getMultiLayerTransformer().transform(
						viewer.getGraphLayout().transform(qVertex));

				Labeler labeler = null;
				String label = null;
				if (qVertex.isBoundaryVertex()) {
					label = qVertex.getCoreName();
					labeler = components.get(qVertex);
					if (labeler == null) {
						labeler = new Labeler(label);
						components.put(qVertex, labeler);
						viewer.add(labeler);
						Color colour = new Color(0, 0, 0, 0);
						labeler.setColor(colour);
						labeler.addChangeListener(new ChangeListener() {

							public void stateChanged(ChangeEvent e) {
								Labeler lab = (Labeler) e.getSource();
								if (qVertex != null) {
									try {
										String newN = lab.getText();
										String oldN = qVertex.getCoreName();
										cacheVertexPositions();
										String[] names = core.renameVertex(getGraph(), oldN, newN);
										if (names.length == 2) {
											Point2D oldP = verticesCache.get(newN);
											verticesCache.put(names[1], oldP);
											verticesCache.remove(newN);
										}
										Point2D oldP = verticesCache.get(oldN);
										verticesCache.put(newN, oldP);
										verticesCache.remove(oldN);
										Rectangle2D rect = new Rectangle2D.Double(viewer.getGraphLayout().getSize().width,
												0, 20, viewer.getGraphLayout().getSize().height);
										updateGraph(rect);
									} catch (CoreException err) {
										errorDialog(err.getMessage());
									}
								}
							}
						});
					}
				} else {
					label = qVertex.getData().getStringValue();
					// lazily create the labeler
					labeler = components.get(qVertex);
					if (labeler == null) {
						labeler = new Labeler(qVertex.getVertexType().getDataType(), label);
						components.put(qVertex, labeler);
						viewer.add(labeler);
						Color colour = qVertex.getVertexType().getVisualizationData().getLabelColour();
						if (colour != null) {
							labeler.setColor(colour);
						}
						labeler.addChangeListener(new ChangeListener() {

							public void stateChanged(ChangeEvent e) {
								Labeler lab = (Labeler) e.getSource();
								if (qVertex != null) {
									try {
										core.setVertexAngle(getGraph(), qVertex, lab.getText());
									} catch (CoreException err) {
										coreErrorMessage("The label could not be updated", err);
									}
								}
							}
						});
					}
				}

				labeler.setText(label);

				Rectangle rect = new Rectangle(labeler.getPreferredSize());
				Point loc = new Point((int) (screen.getX() - rect.getCenterX()),
						(int) screen.getY() + 10);
				rect.setLocation(loc);

				if (!labeler.getBounds().equals(rect)) {
					labeler.setBounds(rect);
				}

				return dummyLabel;
			} else if (value != null) {
				realLabel.setText(value.toString());
				return realLabel;
			} else {
				return dummyLabel;
			}
		}

		/**
		 * Removes orphaned labels.
		 */
		public void cleanup() {
			final Map<Vertex, Labeler> oldComponents = components;
			components = new HashMap<Vertex, Labeler>();
			for (Labeler l : oldComponents.values()) {
				viewer.remove(l);
			}
		}
	}

	private class QBangBoxLabeler implements BangBoxLabelRenderer {

		Map<BangBox, Labeler> components;
		JLabel dummyLabel = new JLabel();
		JLabel realLabel = new JLabel();

		public QBangBoxLabeler() {
			components = new HashMap<BangBox, Labeler>();
			realLabel.setOpaque(true);
			realLabel.setBackground(Color.white);
		}

		public <T> Component getBangBoxLabelRendererComponent(JComponent vv,
				Object value, Font font, boolean isSelected, T bb) {
			if (bb instanceof BangBox) {
				final BangBox qBb = (BangBox) bb;

				//FIXME: This method is called a lot, it would probably be nicer
				//to store a map: BB -> Shape, so we compute the position of the
				//label directly from the shape of the BB and avoid all that min/max
				//thing.
				if (!getGraph().containsBangBox(qBb)) {
					return dummyLabel;
				}
				Collection<Vertex> bangedV = getGraph().getBoxedVertices(qBb);
				if (bangedV.isEmpty()) {
					return dummyLabel;
				}
				Point2D screen = new Point2D.Double(0, 0);
				SortedSet<Double> Xs = new TreeSet<Double>();
				SortedSet<Double> Ys = new TreeSet<Double>();

				for (Vertex v : bangedV) {
					Point2D p = viewer.getRenderContext().
							getMultiLayerTransformer().transform(
							viewer.getGraphLayout().transform(v));
					Xs.add(p.getX());
					Ys.add(p.getY());
				}
				screen.setLocation((Xs.last() - Xs.first()) / 2 + Xs.first(), Ys.first());
				Labeler labeler = null;
				String label = null;
				label = qBb.getCoreName();
				labeler = components.get(qBb);
				if (labeler == null) {
					labeler = new Labeler(label);
					components.put(qBb, labeler);
					viewer.add(labeler);
					Color colour = new Color(0, 0, 0, 0);
					labeler.setColor(colour);
					labeler.addChangeListener(new ChangeListener() {

						public void stateChanged(ChangeEvent e) {
							Labeler lab = (Labeler) e.getSource();
							if (qBb != null) {
								try {
									String newN = lab.getText();
									String oldN = qBb.getCoreName();
									core.renameBangBox(getGraph(), oldN, newN);
									qBb.updateCoreName(newN);
								} catch (CoreException err) {
									errorDialog(err.getMessage());
								}
							}
						}
					});
				}

				labeler.setText(label);

				Rectangle rect = new Rectangle(labeler.getPreferredSize());
				Point loc = new Point((int) (screen.getX() - rect.getCenterX()),
						(int) screen.getY() - 30);
				rect.setLocation(loc);

				if (!labeler.getBounds().equals(rect)) {
					labeler.setBounds(rect);
				}

				return dummyLabel;
			} else if (value != null) {
				realLabel.setText(value.toString());
				return realLabel;
			} else {
				return dummyLabel;
			}
		}

		/**
		 * Removes orphaned labels.
		 */
		public void cleanup() {
			final Map<BangBox, Labeler> oldComponents = components;
			components = new HashMap<BangBox, Labeler>();
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
			add(new ScalingGraphMousePlugin(new ViewScalingControl(), QuantoApp.COMMAND_MASK));
			add(new ViewTranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | QuantoApp.COMMAND_MASK));
			ViewScrollingGraphMousePlugin scrollerPlugin = new ViewScrollingGraphMousePlugin();
			scrollerPlugin.setShift(10.0);
			add(scrollerPlugin);
			add(new AddEdgeGraphMousePlugin<Vertex, Edge>(
					viewer,
					InteractiveGraphView.this,
					InputEvent.BUTTON1_MASK | InputEvent.ALT_MASK));
			pickingMouse = new ConstrainedPickingBangBoxGraphMousePlugin<Vertex, Edge, BangBox>(20.0, 20.0) {
				// don't change the cursor

				@Override
				public void mouseEntered(MouseEvent e) {
				}

				@Override
				public void mouseExited(MouseEvent e) {
				}

				@Override
				public void mouseReleased(MouseEvent e) {
					super.mouseReleased(e);
					setVerticesPositionData();
				}
			};
			edgeMouse = new AddEdgeGraphMousePlugin<Vertex, Edge>(
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

		final public void setPickingMouse() {
			clearMouse();
			pickingMouseActive = true;
			add(pickingMouse);
			InteractiveGraphView.this.repaint();
			if (isAttached()) {
				getViewPort().setCommandStateSelected(CommandManager.Command.SelectMode, true);
			}
		}

		public void setEdgeMouse() {
			clearMouse();
			edgeMouseActive = true;
			add(edgeMouse);
			InteractiveGraphView.this.repaint();
			if (isAttached()) {
				if (directedEdges) {
					getViewPort().setCommandStateSelected(CommandManager.Command.DirectedEdgeMode, true);
				} else {
					getViewPort().setCommandStateSelected(CommandManager.Command.UndirectedEdgeMode, true);
				}
			}
		}

		public boolean isPickingMouse() {
			return pickingMouseActive;
		}

		public boolean isEdgeMouse() {
			return edgeMouseActive;
		}
	}

	public InteractiveGraphView(Core core, CoreGraph g) throws CoreException {
		this(core, g, new Dimension(800, 600));
	}

	public InteractiveGraphView(Core core, CoreGraph g, Dimension size) throws CoreException {
		super(new BorderLayout(), g.getCoreName());
		setPreferredSize(size);
		initLayout = new QuantoDotLayout(g);
		initLayout.initialize();
		forceLayout = new QuantoForceLayout(g, initLayout, 20.0);
		smoothLayout = new SmoothLayoutDecorator<Vertex, Edge>(forceLayout);
		viewer = new GraphVisualizationViewer(smoothLayout);

		/* This is probably not the place to do it:
		 * get vertices user data from graph, and set
		 * position.*/
		Map<String, Vertex> vmap = g.getVertexMap();
		for (String key : vmap.keySet()) {
			PositionGraphUserDataSerializer pds = new PositionGraphUserDataSerializer(core.getTalker());
			Point2D p = (Point2D) pds.getVertexUserData(g, key);
			if (p != null) {
				viewer.getGraphLayout().setLocation(vmap.get(key), p);
				viewer.getGraphLayout().lock(vmap.get(key), true);
			}
		}
		add(new ViewZoomScrollPane(viewer), BorderLayout.CENTER);

		this.core = core;
		Relaxer r = viewer.getModel().getRelaxer();
		if (r != null) {
			r.setSleepTime(10);
		}

		graphMouse = new RWMouse();
		viewer.setGraphMouse(graphMouse);

		viewer.getRenderContext().getMultiLayerTransformer().setTransformer(Layer.VIEW, new ConstrainedMutableAffineTransformer());
		viewer.getRenderContext().getMultiLayerTransformer().setTransformer(Layer.LAYOUT, new ConstrainedMutableAffineTransformer());

		viewer.addPreRenderPaintable(new VisualizationServer.Paintable() {

			public void paint(Graphics g) {
				Color old = g.getColor();
				g.setColor(Color.red);
				if ((graphMouse.isEdgeMouse()) && (directedEdges)) {
					g.drawString("DIRECTED EDGE MODE", 5, 15);
				} else if (graphMouse.isEdgeMouse()) {
					g.drawString("UNDIRECTED EDGE MODE", 5, 15);
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

		viewer.getRenderContext().setVertexDrawPaintTransformer(
				new Transformer<Vertex, Paint>() {

					public Paint transform(Vertex v) {
						if (isVertexLocked(v)) {
							return Color.gray;
						} else {
							return Color.black;
						}
					}
				});
		viewer.getRenderer().setVertexRenderer(new QVertexRenderer() {

			@Override
			public void paintVertex(RenderContext<Vertex, Edge> rc, Layout<Vertex, Edge> layout, Vertex v) {
				if (rc.getPickedVertexState().isPicked(v)) {
					Rectangle bounds = rc.getVertexShapeTransformer().transform(v).getBounds();
					Point2D p = layout.transform(v);
					p = rc.getMultiLayerTransformer().transform(Layer.LAYOUT, p);
					float x = (float) p.getX();
					float y = (float) p.getY();
					// create a transform that translates to the location of
					// the vertex to be rendered
					AffineTransform xform = AffineTransform.getTranslateInstance(x, y);
					// transform the vertex shape with xtransform
					bounds = xform.createTransformedShape(bounds).getBounds();
					bounds.translate(-1, -1);

					GraphicsDecorator g = rc.getGraphicsContext();
					bounds.grow(3, 3);
					g.setColor(new Color(200, 200, 255));
					g.fillRoundRect(bounds.x, bounds.y, bounds.width, bounds.height, 4, 4);
					g.setColor(Color.BLUE);
					g.drawRoundRect(bounds.x, bounds.y, bounds.width, bounds.height, 4, 4);
				}
				super.paintVertex(rc, layout, v);
			}
		});

		viewer.getRenderContext().setVertexLabelRenderer(new QVertexLabeler());
		// increase the picksize
		viewer.getRenderContext().setBangBoxLabelRenderer(new QBangBoxLabeler());
		viewer.setPickSupport(new ShapeBangBoxPickSupport<Vertex, Edge, BangBox>(viewer, 4));
		viewer.setBoundingBoxEnabled(false);

		buildActionMap();

		g.addChangeListener(new ChangeListener() {

			public void stateChanged(ChangeEvent e) {
				if (saveEnabled && isAttached()) {
					getViewPort().setCommandEnabled(CommandManager.Command.Save,
							!getGraph().isSaved());
					firePropertyChange("saved", !getGraph().isSaved(), getGraph().isSaved());
				}
			}
		});
	}

	public boolean isVertexLocked(Vertex v) {
		return viewer.getGraphLayout().isLocked(v);
	}

	public void lockVertices(Collection<Vertex> verts) {
		for (Vertex v : verts) {
			viewer.getGraphLayout().lock(v, true);
		}
	}

	public void unlockVertices(Collection<Vertex> verts) {
		for (Vertex v : verts) {
			viewer.getGraphLayout().lock(v, false);
		}
	}

	public boolean isSaveEnabled() {
		return saveEnabled;
	}

	public void setSaveEnabled(boolean saveEnabled) {
		if (this.saveEnabled != saveEnabled) {
			this.saveEnabled = saveEnabled;
			if (isAttached()) {
				getViewPort().setCommandEnabled(
						CommandManager.Command.Save,
						saveEnabled && !isSaved());
			}
			if (saveEnabled) {
				actionMap.put(CommandManager.Command.Save.toString(), new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						saveGraph();
					}
				});
			} else {
				actionMap.remove(CommandManager.Command.Save.toString());
			}
		}
	}

	public boolean isSaveAsEnabled() {
		return saveAsEnabled;
	}

	public void setSaveAsEnabled(boolean saveAsEnabled) {
		if (this.saveAsEnabled != saveAsEnabled) {
			this.saveAsEnabled = saveAsEnabled;
			if (isAttached()) {
				getViewPort().setCommandEnabled(
						CommandManager.Command.SaveAs,
						saveAsEnabled);
			}
			if (saveAsEnabled) {
				actionMap.put(CommandManager.Command.SaveAs.toString(), new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						saveGraphAs();
					}
				});
			} else {
				actionMap.remove(CommandManager.Command.SaveAs.toString());
			}
		}
	}

	public GraphVisualizationViewer getVisualization() {
		return viewer;
	}

	public void addChangeListener(ChangeListener listener) {
		viewer.addChangeListener(listener);
	}

	public CoreGraph getGraph() {
		return viewer.getGraph();
	}

	protected static ImageIcon createImageIcon(String path) {
		java.net.URL imgURL = InteractiveGraphView.class.getResource(path);
		if (imgURL != null) {
			return new ImageIcon(imgURL);
		} else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}

	private class JobEndEvent extends EventObject {

		private boolean aborted = false;

		public JobEndEvent(Object source) {
			super(source);
		}

		public JobEndEvent(Object source, boolean aborted) {
			super(source);
			this.aborted = aborted;
		}

		public boolean jobWasAborted() {
			return aborted;
		}
	}

	private interface JobListener extends EventListener {

		/**
		 * Notifies the listener that the job has terminated.
		 *
		 * Guaranteed to be sent exactly once in the life of a job.
		 * @param event
		 */
		void jobEnded(JobEndEvent event);
	}

	/**
	 * A separate thread that executes some job on the graph
	 * asynchronously.
	 *
	 * This mainly exists to allow the job to be displayed to the user
	 * and aborted.
	 *
	 * The job must call fireJobFinished() when it has come to a natural
	 * end.  It may also call fireJobAborted() when it is interrupted,
	 * but should work fine even if it doesn't.
	 */
	private abstract class Job extends Thread {

		private EventListenerList listenerList = new EventListenerList();
		private JobEndEvent jobEndEvent = null;

		/**
		 * Abort the job.  The default implementation interrupts the
		 * thread and calls fireJobAborted().
		 */
		public void abortJob() {
			this.interrupt();
			fireJobAborted();
		}

		/**
		 * Add a job listener.
		 *
		 * All job listener methods execute in the context of the
		 * AWT event queue.
		 * @param l
		 */
		public void addJobListener(JobListener l) {
			listenerList.add(JobListener.class, l);
		}

		public void removeJobListener(JobListener l) {
			listenerList.remove(JobListener.class, l);
		}

		/**
		 * Notify listeners that the job has finished successfully,
		 * if no notification has already been sent.
		 */
		protected final void fireJobFinished() {
			if (jobEndEvent == null) {
				fireJobEnded(false);
			}
		}

		/**
		 * Notify listeners that the job has been aborted, if no
		 * notification has already been sent.
		 */
		protected final void fireJobAborted() {
			if (jobEndEvent == null) {
				fireJobEnded(true);
			}
		}

		private void fireJobEnded(final boolean aborted) {
			SwingUtilities.invokeLater(new Runnable() {

				public void run() {
					// Guaranteed to return a non-null array
					Object[] listeners = listenerList.getListenerList();
					// Process the listeners last to first, notifying
					// those that are interested in this event
					for (int i = listeners.length - 2; i >= 0; i -= 2) {
						if (listeners[i] == JobListener.class) {
							// Lazily create the event:
							if (jobEndEvent == null) {
								jobEndEvent = new JobEndEvent(this, aborted);
							}
							((JobListener) listeners[i + 1]).jobEnded(jobEndEvent);
						}
					}
				}
			});
		}
	}

	private class JobIndicatorPanel extends JPanel {

		private JLabel textLabel;
		private JButton cancelButton = null;

		public JobIndicatorPanel(String description, final Job job) {
			super(new BorderLayout());

			setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
			setBackground(UIManager.getColor("textHighlight"));

			textLabel = new JLabel(description);
			add(textLabel, BorderLayout.CENTER);

			cancelButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/Stop16.gif"));
			cancelButton.setToolTipText("Abort this operation");
			cancelButton.setMargin(new Insets(0, 0, 0, 0));
			cancelButton.addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					job.abortJob();
				}
			});
			add(cancelButton, BorderLayout.LINE_END);
		}
	}

	/**
	 * Registers a job, allowing it to be aborted by the "Abort all"
	 * action.
	 *
	 * Does not need to be called for a job if showJobIndicator() is called
	 * for that job.
	 * @param job
	 */
	private void registerJob(final Job job) {
		if (activeJobs == null) {
			activeJobs = new LinkedList<Job>();
		}
		activeJobs.add(job);
		if (getViewPort() != null) {
			getViewPort().setCommandEnabled(CommandManager.Command.Abort, true);
		}
		job.addJobListener(new JobListener() {

			public void jobEnded(JobEndEvent event) {
				activeJobs.remove(job);
				if (activeJobs.isEmpty() && getViewPort() != null) {
					getViewPort().setCommandEnabled(CommandManager.Command.Abort, false);
				}
			}
		});
	}

	/**
	 * Shows an indicator at the bottom of the view with (optionally)
	 * a button to cancel the job.
	 *
	 * @param jobDescription  The text on the indicator
	 * @param cancelListener  Called when the user cancels the job
	 *                        (if null, no cancel button is shown)
	 */
	private void showJobIndicator(String jobDescription, Job job) {
		registerJob(job);
		if (indicatorPanel == null) {
			indicatorPanel = new JPanel();
			indicatorPanel.setLayout(new BoxLayout(indicatorPanel, BoxLayout.PAGE_AXIS));
			add(indicatorPanel, BorderLayout.PAGE_END);
		}
		final JobIndicatorPanel indicator = new JobIndicatorPanel(jobDescription, job);
		indicatorPanel.add(indicator);
		indicatorPanel.validate();
		InteractiveGraphView.this.validate();
		job.addJobListener(new JobListener() {

			public void jobEnded(JobEndEvent event) {
				indicatorPanel.remove(indicator);
				InteractiveGraphView.this.validate();
			}
		});
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

	public void addEdge(Vertex s, Vertex t) {
		try {
			core.addEdge(getGraph(), directedEdges, s, t);
		} catch (CoreException e) {
			coreErrorDialog("Could not add a directed edge", e);
		}
	}

	public void addBoundaryVertex() {
		try {
			core.addBoundaryVertex(getGraph());
			setVerticesPositionData();
		} catch (CoreException e) {
			coreErrorDialog("Could not add a boundary vertex", e);
		}
	}

	public void addVertex(String type) {
		try {
			core.addVertex(getGraph(), type);
			setVerticesPositionData();
		} catch (CoreException e) {
			coreErrorDialog("Could not add a vertex", e);
		}
	}

	public void showRewrites() {
		try {
			Set<Vertex> picked = viewer.getPickedVertexState().getPicked();
			if (picked.isEmpty()) {
				core.attachRewrites(getGraph(), getGraph().getVertices());
			} else {
				core.attachRewrites(getGraph(), picked);
			}
			JFrame rewrites = new RewriteViewer(InteractiveGraphView.this);
			rewrites.setVisible(true);
		} catch (CoreException e) {
			coreErrorDialog("Could not obtain the rewrites", e);
		}
	}

	public void removeOldLabels() {
		((QVertexLabeler) viewer.getRenderContext().getVertexLabelRenderer()).cleanup();
		((QBangBoxLabeler) viewer.getRenderContext().getBangBoxLabelRenderer()).cleanup();
	}

	public void cleanUp() {
		removeOldLabels();
		((QVertexLabeler) viewer.getRenderContext().getVertexLabelRenderer()).cleanup();
		((QBangBoxLabeler) viewer.getRenderContext().getBangBoxLabelRenderer()).cleanup();
		if (saveEnabled && isAttached()) {
			getViewPort().setCommandEnabled(CommandManager.Command.Save,
					!getGraph().isSaved());
		}
	}

	public void cacheVertexPositions() {
		verticesCache = new HashMap<String, Point2D>();
		for (Vertex v : getGraph().getVertices()) {
			int X = (int) smoothLayout.getDelegate().transform(v).getX();
			int Y = (int) smoothLayout.getDelegate().transform(v).getY();
			Point2D p = new Point2D.Double(X, Y);
			verticesCache.put(v.getCoreName(), p);
		}
	}

	public void setVertexPositionData(Vertex v) {
		try {
			core.startUndoGroup(getGraph());
			PositionGraphUserDataSerializer pds = new PositionGraphUserDataSerializer(core.getTalker());
			int X = (int) smoothLayout.getDelegate().transform(v).getX();
			int Y = (int) smoothLayout.getDelegate().transform(v).getY();
			Point2D new_p = new Point2D.Double(X, Y);
			pds.setVertexUserData(getGraph(), v.getCoreName(), new_p);
			core.endUndoGroup(getGraph());
		} catch (CoreException e) {
			errorDialog(e.getMessage());
		}
	}

	public void setVerticesPositionData() {
		CoreGraph graph = getGraph();
		PositionGraphUserDataSerializer pds = new PositionGraphUserDataSerializer(core.getTalker());
		try {
			//New vertices are added but not pushed on the undo stack 
			core.startUndoGroup(graph);
			for (Vertex v : graph.getVertices()) {
				int X = (int) smoothLayout.getDelegate().transform(v).getX();
				int Y = (int) smoothLayout.getDelegate().transform(v).getY();
				Point2D old_p = (Point2D) pds.getVertexUserData(graph, v.getCoreName());
				Point2D new_p = new Point2D.Double(X, Y);
				if (old_p == null) {
					pds.setVertexUserData(graph, v.getCoreName(), new_p);
				}
			}
			core.endUndoGroup(graph);

			ArrayList<Vertex> vertices = new ArrayList<Vertex>();
			for (Vertex v : graph.getVertices()) {
				int X = (int) smoothLayout.getDelegate().transform(v).getX();
				int Y = (int) smoothLayout.getDelegate().transform(v).getY();
				Point2D old_p = (Point2D) pds.getVertexUserData(graph, v.getCoreName());
				Point2D new_p = new Point2D.Double(X, Y);
				if (old_p.distance(new_p) > 1.5) {
					vertices.add(v);
				}
			}
			if (vertices.size() > 0) {
				//The first one creates an undo point
				Vertex v = vertices.get(0);
				int X = (int) smoothLayout.getDelegate().transform(v).getX();
				int Y = (int) smoothLayout.getDelegate().transform(v).getY();
				Point2D new_p = new Point2D.Double(X, Y);
				pds.setVertexUserData(graph, v.getCoreName(), new_p);
				vertices.remove(v);
			}
			if (vertices.size() <= 0) {
				return;
			}
			//The others do not
			core.startUndoGroup(graph);
			for (Vertex v : vertices) {
				int X = (int) smoothLayout.getDelegate().transform(v).getX();
				int Y = (int) smoothLayout.getDelegate().transform(v).getY();
				Point2D new_p = new Point2D.Double(X, Y);
				pds.setVertexUserData(graph, v.getCoreName(), new_p);
			}
			core.endUndoGroup(graph);
		} catch (CoreException e) {
			errorDialog(e.getMessage());
		}
	}

	public void updateGraph(Rectangle2D rewriteRect) throws CoreException {
		core.updateGraph(getGraph());
		int count = 0;
		for (Vertex v : getGraph().getVertices()) {
			if (verticesCache.get(v.getCoreName()) != null) {
				PositionGraphUserDataSerializer pds = new PositionGraphUserDataSerializer(core.getTalker());
				Point2D p = (Point2D) pds.getVertexUserData(getGraph(), v.getCoreName());
				if (p != null) {
					viewer.getGraphLayout().setLocation(v, p);
				} else {
					viewer.getGraphLayout().setLocation(v, verticesCache.get(v.getCoreName()));
				}
				viewer.getGraphLayout().lock(v, true);
			} else {
				if (rewriteRect != null) {
					PositionGraphUserDataSerializer pds = new PositionGraphUserDataSerializer(core.getTalker());
					Point2D p = (Point2D) pds.getVertexUserData(getGraph(), v.getCoreName());
					if (p != null) {
						viewer.getGraphLayout().setLocation(v, p);
						viewer.getGraphLayout().lock(v, true);
					} else {
						viewer.shift(rewriteRect, v, new Point2D.Double(0, 20 * count));
						setVertexPositionData(v);
						count++;
					}
				}
			}
		}
		forceLayout.startModify();
		viewer.modifyLayout();
		forceLayout.endModify();
		removeOldLabels();
		viewer.update();
		//locking and unlocking used internally to notify the layout which vertices have user data
		unlockVertices(getGraph().getVertices());
	}

	public void outputToTextView(String text) {
		TextView tview = new TextView(getTitle() + "-output", text);
		getViewManager().addView(tview);

		if (isAttached()) {
			getViewPort().openView(tview);
		}
	}
	private SubgraphHighlighter highlighter = null;

	public void clearHighlight() {
		if (highlighter != null) {
			viewer.removePostRenderPaintable(highlighter);
		}
		highlighter = null;
		viewer.repaint();
	}

	public void highlightSubgraph(CoreGraph g) {
		clearHighlight();
		highlighter = new SubgraphHighlighter(g);
		viewer.addPostRenderPaintable(highlighter);
		viewer.update();
	}

	public void startRewriting() {
		abortRewriting();
		rewriter = new RewriterJob();
		rewriter.addJobListener(new JobListener() {

			public void jobEnded(JobEndEvent event) {
				if (rewriter != null) {
					rewriter = null;
				}
				if (isAttached()) {
					setupNormaliseAction(getViewPort());
				}
			}
		});
		rewriter.start();
		showJobIndicator("Rewriting...", rewriter);
		if (isAttached()) {
			setupNormaliseAction(getViewPort());
		}
	}

	public void abortRewriting() {
		if (rewriter != null) {
			rewriter.abortJob();
			rewriter = null;
		}
	}

	private void setupNormaliseAction(ViewPort vp) {
		if (rewriter == null) {
			vp.setCommandEnabled(CommandManager.Command.Normalise, true);
		} else {
			vp.setCommandEnabled(CommandManager.Command.Normalise, false);
		}
	}

	private class RewriterJob extends Job {

		private boolean highlight = false;

		private void attachNextRewrite() {
			try {
				core.attachOneRewrite(
						getGraph(),
						getGraph().getVertices());
			} catch (CoreException e) {
				coreErrorDialog("Could not attach the next rewrite", e);
			}
		}

		private void attachAllRewrites() {
			try {
				core.attachRewrites(getGraph(), getGraph().getVertices());
			} catch (CoreException e) {
				coreErrorDialog("Could not attach the next rewrite", e);
			}
		}

		private void invokeHighlightSubgraphAndWait(CoreGraph subgraph)
				throws InterruptedException {
			highlight = true;
			final CoreGraph fSubGraph = subgraph;
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
			} catch (InvocationTargetException ex) {
				logger.log(Level.WARNING,
						"invoke and wait failed", ex);
			}
		}

		@Override
		public void run() {
			try {
				// FIXME: communicating with the core: is this
				//        really threadsafe?  Probably not.

				attachAllRewrites();
				List<AttachedRewrite<CoreGraph>> rws = getRewrites();
				int count = 0;
				int rw = 0;
				while (rws.size() > 0
						&& !Thread.interrupted()) {
					invokeHighlightSubgraphAndWait(rws.get(rw).getNewGraph());
					sleep(1500);
					invokeApplyRewriteAndWait(rw);
					++count;
					attachAllRewrites();
					rws = getRewrites();
				}

				fireJobFinished();
				invokeInfoDialogAndWait("Applied " + count + " rewrites");
			} catch (InterruptedException e) {
				if (highlight) {
					invokeClearHighlightLater();
				}
			}
		}
	}

	private class SubgraphHighlighter
			implements VisualizationServer.Paintable {

		Collection<Vertex> verts;

		public SubgraphHighlighter(CoreGraph g) {
			verts = getGraph().getSubgraphVertices(g);
		}

		public void paint(Graphics g) {
			Color oldColor = g.getColor();
			g.setColor(Color.blue);
			Graphics2D g2 = (Graphics2D) g.create();
			float opac = 0.3f + 0.2f * (float) Math.sin(
					System.currentTimeMillis() / 150.0);
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opac));

			for (Vertex v : verts) {
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
	 * Gets the attached rewrites as a list of Pair<QGraph>. Returns and empty
	 * list on console error.
	 * @return
	 */
	public List<AttachedRewrite<CoreGraph>> getRewrites() {
		try {
			rewriteCache = core.getAttachedRewrites(getGraph());
			return rewriteCache;
		} catch (CoreException e) {
			coreErrorDialog("Could not obtain the rewrites", e);
		}

		return new ArrayList<AttachedRewrite<CoreGraph>>();
	}

	public void applyRewrite(int index) {
		Rectangle2D rewriteRect = new Rectangle2D.Double();
		try {
			if (rewriteCache != null && rewriteCache.size() > index) {
				viewer.setCoreGraph(rewriteCache.get(index).getGraph());
				List<Vertex> sub = getGraph().getSubgraphVertices(
						(CoreGraph) rewriteCache.get(index).getNewGraph());
				if (sub.size() > 0) {
					rewriteRect = viewer.getSubgraphBounds(sub);
					if (sub.size() == 1) {
						smoothLayout.setOrigin(rewriteRect.getCenterX(), rewriteRect.getCenterY());
					}
				}
			}
			core.applyAttachedRewrite(getGraph(), index);
			cacheVertexPositions();
			getGraph().updateGraph(rewriteCache.get(index).getNewGraph());
			updateGraph(rewriteRect);
			smoothLayout.setOrigin(0, 0);
		} catch (CoreException e) {
			coreErrorDialog("Could not apply the rewrite", e);
		}
	}

	public Core getCore() {
		return core;
	}

	public void commandTriggered(String command) {
		ActionListener listener = actionMap.get(command);
		if (listener != null) {
			listener.actionPerformed(new ActionEvent(this, -1, command));
		}
	}

	public void saveGraphAs() {
		File f = QuantoApp.getInstance().saveFile(this);
		if (f != null) {
			try {
				core.saveGraph(getGraph(), f);
				core.renameGraph(getGraph(), f.getName());
				getGraph().setFileName(f.getAbsolutePath());
				getGraph().setSaved(true);
				firePropertyChange("saved", !getGraph().isSaved(), getGraph().isSaved());
				setTitle(f.getName());
			} catch (CoreException e) {
				coreErrorDialog("Could not save the graph", e);
			} catch (IOException e) {
				detailedErrorDialog("Save Graph", "Could not save the graph", e.getLocalizedMessage());
			}
		}
	}

	public void saveGraph() {
		if (getGraph().getFileName() != null) {
			try {
				core.saveGraph(getGraph(), new File(getGraph().getFileName()));
				getGraph().setSaved(true);
				firePropertyChange("saved", !getGraph().isSaved(), getGraph().isSaved());
			} catch (CoreException e) {
				coreErrorDialog("Could not save the graph", e);
			} catch (IOException e) {
				detailedErrorDialog("Save Graph", "Could not save the graph", e.getLocalizedMessage());
			}
		} else {
			saveGraphAs();
		}
	}

	public static void registerKnownCommands(Core core, CommandManager commandManager) {
		/*
		 * Add commands dynamically and add registered vertex types
		 */
		for (VertexType vertexType : core.getActiveTheory().getVertexTypes()) {
			commandManager.registerCommand("add-" + vertexType.getTypeName() + "-vertex-command");
		}
	}

	private void buildActionMap() {
		actionMap.put(CommandManager.Command.Save.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				saveGraph();
			}
		});
		actionMap.put(CommandManager.Command.SaveAs.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				saveGraphAs();
			}
		});

		actionMap.put(CommandManager.Command.Undo.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = viewer.getGraphBounds();
					core.undo(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not undo", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.Redo.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = new Rectangle2D.Double(viewer.getGraphLayout().getSize().width,
							0, 20, viewer.getGraphLayout().getSize().height);
					core.redo(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not redo", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.UndoRewrite.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = viewer.getGraphBounds();
					core.undoRewrite(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not undo", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.RedoRewrite.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = new Rectangle2D.Double(viewer.getGraphLayout().getSize().width,
							0, 20, viewer.getGraphLayout().getSize().height);
					core.redoRewrite(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not redo", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.Cut.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					Set<Vertex> picked = viewer.getPickedVertexState().getPicked();
					if (!picked.isEmpty()) {
						core.cutSubgraph(getGraph(), picked);
						removeOldLabels();
					}
				} catch (CoreException ex) {
					coreErrorDialog("Could not cut selection", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.Copy.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					Set<Vertex> picked = viewer.getPickedVertexState().getPicked();
					if (!picked.isEmpty()) {
						core.copySubgraph(getGraph(), picked);
					}
				} catch (CoreException ex) {
					coreErrorDialog("Could not copy selection", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.Paste.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = new Rectangle2D.Double(viewer.getGraphLayout().getSize().width,
							0, 20, viewer.getGraphLayout().getSize().height);
					core.paste(getGraph());
					/* 
					 * FIXME: maybe, this is not the right place? 
					 * When we paste a graph we want to keep it's layout as well, so we get it's quanto-position uidata
					 * and translate everything so that it ends up at the right of the current graph.
					 * What we get is a graph, already merged and with fresh names. So in order to know which one 
					 * were copied we check the "copy_of" user_data which is set automatically by the core when a
					 * subgraph get copied, and delete it afterwards.
					 *  */
					CopyOfGraphUserDataSerializer cos = new CopyOfGraphUserDataSerializer(core.getTalker());
					PositionGraphUserDataSerializer pos = new PositionGraphUserDataSerializer(core.getTalker());

					core.startUndoGroup(getGraph());
					for (Vertex v : getGraph().getVertices()) {
						String copy_of_vertex = (String) cos.getVertexUserData(getGraph(), v.getCoreName());
						if ((copy_of_vertex != null) && (!copy_of_vertex.equals(""))) {
							//Then translate its quanto-gui:position
							Point2D position = (Point2D) pos.getVertexUserData(getGraph(), v.getCoreName());
							position.setLocation(position.getX() + rect.getCenterX() + 20, position.getY());
							pos.setVertexUserData(getGraph(), v.getCoreName(), position);
							cos.deleteVertexUserData(getGraph(), v.getCoreName());
						}
					}
					/* For now we do nothing with Edge and !-Boxes user data but still need to remove their "copy_of" UD */
					for (Edge edge : getGraph().getEdges()) {
						String copy_of = (String) cos.getEdgeUserData(getGraph(), edge.getCoreName());
						if ((copy_of != null) && (!copy_of.equals(""))) {
							cos.deleteEdgeUserData(getGraph(), edge.getCoreName());
						}
					}
					for (BangBox bb : getGraph().getBangBoxes()) {
						String copy_of = (String) cos.getBangBoxUserData(getGraph(), bb.getCoreName());
						if ((copy_of != null) && (!copy_of.equals(""))) {
							cos.deleteBangBoxUserData(getGraph(), bb.getCoreName());
						}
					}
					core.endUndoGroup(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not paste selection", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.SelectAll.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				synchronized (getGraph()) {
					for (Vertex v : getGraph().getVertices()) {
						viewer.getPickedVertexState().pick(v, true);
					}
				}
			}
		});
		actionMap.put(CommandManager.Command.DeselectAll.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewer.getPickedVertexState().clear();
			}
		});
		actionMap.put(CommandManager.Command.Relayout.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				// re-layout
				initLayout.reset();
				forceLayout.forgetPositions();
				viewer.update();
				setVerticesPositionData();
			}
		});

		actionMap.put(CommandManager.Command.ExportToPdf.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {

					File outputFile = QuantoApp.getInstance().saveFile(InteractiveGraphView.this);
					if (outputFile != null) {
						OutputStream file = new FileOutputStream(outputFile);
						PdfGraphVisualizationServer server = new PdfGraphVisualizationServer(core.getActiveTheory(), getGraph());
						server.renderToPdf(file);
						file.close();
					}
				} catch (DocumentException ex) {
					detailedErrorMessage("Export to PDF", "Could not generate the PDF", ex.getLocalizedMessage());
				} catch (IOException ex) {
					detailedErrorMessage("Export to PDF", "Could not save the PDF", ex.getLocalizedMessage());
				}
			}
		});
		actionMap.put(CommandManager.Command.SelectMode.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				graphMouse.setPickingMouse();
			}
		});
		actionMap.put(CommandManager.Command.DirectedEdgeMode.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				directedEdges = true;
				graphMouse.setEdgeMouse();
			}
		});
		actionMap.put(CommandManager.Command.UndirectedEdgeMode.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				directedEdges = false;
				graphMouse.setEdgeMouse();
			}
		});
		actionMap.put(CommandManager.Command.LatexToClipboard.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				String tikz = TikzOutput.generate(
						getGraph(),
						viewer.getGraphLayout());
				Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
				StringSelection data = new StringSelection(tikz);
				cb.setContents(data, data);
			}
		});
		actionMap.put(CommandManager.Command.AddBoundaryVertex.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				addBoundaryVertex();
			}
		});
		actionMap.put(CommandManager.Command.ShowRewrites.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				showRewrites();
			}
		});
		actionMap.put(CommandManager.Command.Normalise.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if (rewriter != null) {
					abortRewriting();
				}
				startRewriting();

			}
		});
		actionMap.put(CommandManager.Command.Abort.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if (activeJobs != null && activeJobs.size() > 0) {
					Job[] jobs = activeJobs.toArray(new Job[activeJobs.size()]);
					for (Job job : jobs) {
						job.abortJob();
					}
				}
			}
		});
		actionMap.put(CommandManager.Command.FastNormalise.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = viewer.getGraphBounds();
					core.fastNormalise(getGraph());
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not normalise graph", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.BangVertices.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					core.addBangBox(getGraph(), viewer.getPickedVertexState().getPicked());
				} catch (CoreException ex) {
					coreErrorDialog("Could not add !-box", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.UnbangVertices.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					core.removeVerticesFromBangBoxes(getGraph(), viewer.getPickedVertexState().getPicked());
					updateGraph(null);
				} catch (CoreException ex) {
					coreErrorDialog("Could not remove vertices from !-box", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.DropBangBox.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					core.dropBangBoxes(getGraph(), viewer.getPickedBangBoxState().getPicked());
					removeOldLabels();
				} catch (CoreException ex) {
					coreErrorDialog("Could not remove !-box", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.KillBangBox.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					core.killBangBoxes(getGraph(), viewer.getPickedBangBoxState().getPicked());
					removeOldLabels();
				} catch (CoreException ex) {
					coreErrorDialog("Could not kill !-box", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.DuplicateBangBox.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					cacheVertexPositions();
					Rectangle2D rect = new Rectangle2D.Double(viewer.getGraphLayout().getSize().width,
							0, 20, viewer.getGraphLayout().getSize().height);
					if (viewer.getPickedBangBoxState().getPicked().size() == 1) {
						core.duplicateBangBox(getGraph(), (BangBox) viewer.getPickedBangBoxState().getPicked().toArray()[0]);
					}
					updateGraph(rect);
				} catch (CoreException ex) {
					coreErrorDialog("Could not duplicate !-box", ex);
				}
			}
		});

		actionMap.put(CommandManager.Command.DumpHilbertTermAsText.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					outputToTextView(core.hilbertSpaceRepresentation(getGraph(), Core.RepresentationType.Plain));
				} catch (CoreException ex) {
					coreErrorDialog("Could not create Hilbert term", ex);
				}
			}
		});
		actionMap.put(CommandManager.Command.DumpHilbertTermAsMathematica.toString(), new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				try {
					outputToTextView(core.hilbertSpaceRepresentation(getGraph(), Core.RepresentationType.Mathematica));
				} catch (CoreException ex) {
					coreErrorDialog("Could not create Hilbert term", ex);
				}
			}
		});

		/*
		 * Add dynamically commands corresponding allowing to add registered vertices
		 */
		for (final VertexType vertexType : core.getActiveTheory().getVertexTypes()) {
			actionMap.put("add-" + vertexType.getTypeName() + "-vertex-command", new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					addVertex(vertexType.getTypeName());
				}
			});
		}
	}

	public void attached(ViewPort vp) {
		for (String actionName : actionMap.keySet()) {
			vp.setCommandEnabled(actionName, true);
		}
		if (saveEnabled) {
			vp.setCommandEnabled(CommandManager.Command.Save,
					!getGraph().isSaved());
		}
		if ((graphMouse.isEdgeMouse()) && (directedEdges)) {
			vp.setCommandStateSelected(CommandManager.Command.DirectedEdgeMode, true);
		} else if (graphMouse.isEdgeMouse()) {
			vp.setCommandStateSelected(CommandManager.Command.UndirectedEdgeMode, true);
		} else {
			vp.setCommandStateSelected(CommandManager.Command.SelectMode, true);
		}
		setupNormaliseAction(vp);
		if (activeJobs == null || activeJobs.isEmpty()) {
			vp.setCommandEnabled(CommandManager.Command.Abort, false);
		}
	}

	public void detached(ViewPort vp) {
		vp.setCommandStateSelected(CommandManager.Command.SelectMode, true);

		for (String actionName : actionMap.keySet()) {
			vp.setCommandEnabled(actionName, false);
		}
	}

	@Override
	protected String getUnsavedClosingMessage() {
		return "Graph '" + getGraph().getCoreName() + "' is unsaved. Close anyway?";
	}

	public boolean isSaved() {
		return getGraph().isSaved();
	}

	public void keyPressed(KeyEvent e) {
		// this listener only handles un-modified keys
		if (e.getModifiers() != 0) {
			return;
		}

		int delete = (QuantoApp.isMac) ? KeyEvent.VK_BACK_SPACE : KeyEvent.VK_DELETE;
		if (e.getKeyCode() == delete) {
			try {
				core.deleteEdges(
						getGraph(), viewer.getPickedEdgeState().getPicked());
				core.deleteVertices(
						getGraph(), viewer.getPickedVertexState().getPicked());
				removeOldLabels();

			} catch (CoreException err) {
				coreErrorMessage("Could not delete the vertex", err);
			} finally {
				// if null things are in the picked state, weird stuff
				// could happen.
				viewer.getPickedEdgeState().clear();
				viewer.getPickedVertexState().clear();
			}
		} else {
			switch (e.getKeyCode()) {
				case KeyEvent.VK_B:
					addBoundaryVertex();
					break;
				case KeyEvent.VK_E:
					if (graphMouse.isEdgeMouse()) {
						graphMouse.setPickingMouse();
					} else {
						graphMouse.setEdgeMouse();
					}
					break;
				case KeyEvent.VK_SPACE:
					showRewrites();
					break;
				//hotkey for force layout
				case KeyEvent.VK_A: {
					forceLayout.startModify();
					viewer.modifyLayout();
					forceLayout.endModify();
					setVerticesPositionData();
				}
				break;
			}
			VertexType v = core.getActiveTheory().getVertexTypeByMnemonic(e.getKeyChar());
			if (v != null) {
				addVertex(v.getTypeName());
			}
		}
	}

	public void keyReleased(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	@Override
	public void refresh() {
		// TODO Auto-generated method stub
	}
}
