package quanto.gui;

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
import java.util.Collections;
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
import quanto.gui.QuantoApp.QuantoActionListener;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.contrib.ConstrainedPickingGraphMousePlugin;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.visualization.picking.MultiPickedState;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;

public class InteractiveGraphView extends GraphView
	implements AddEdgeGraphMousePlugin.Adder<QVertex>, InteractiveView
{
	private static final long serialVersionUID = 7196565776978339937L;
	private QuantoCore core;
	private RWMouse graphMouse;
	// these menu items will be added when this view is focused, and removed
	//  when it is unfocused.
	protected List<JMenu> menus;
	private JMenuItem file_saveGraph = null;
	private JMenuItem file_saveGraphAs = null;
	private volatile Thread rewriter = null;
	private List<Rewrite> rewriteCache = null;
	private JRadioButtonMenuItem rbEdgeMode;
	private JRadioButtonMenuItem rbBangBoxMode;
	private JRadioButtonMenuItem rbPickingMode;
	private PickedState<BangBox> pickedBangBoxState;

	public boolean viewHasParent() {
		return this.getParent() != null;
	}

	private class QVertexLabeler implements VertexLabelRenderer
	{
		Map<QVertex, Labeler> components;

		public QVertexLabeler() {
			components = Collections.<QVertex, Labeler>synchronizedMap(
				new HashMap<QVertex, Labeler>());
		}

		public <T> Component getVertexLabelRendererComponent(JComponent vv,
								     Object value, Font font, boolean isSelected, T vertex) {
			if (vertex instanceof QVertex && ((QVertex) vertex).isAngleVertex()) {
				Point2D screen = viewer.getRenderContext().
					getMultiLayerTransformer().transform(
					viewer.getGraphLayout().transform((QVertex) vertex));

				// lazily create the labeler
				Labeler angleLabeler = components.get(vertex);
				if (angleLabeler == null) {
					angleLabeler = new Labeler("");
					components.put((QVertex) vertex, angleLabeler);
					viewer.add(angleLabeler);
					final QVertex qv = (QVertex) vertex;
					if (qv.getColor().equals(Color.red)) {
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

					angleLabeler.addChangeListener(new ChangeListener()
					{
						public void stateChanged(ChangeEvent e) {
							Labeler lab = (Labeler) e.getSource();
							if (qv != null) {
								try {
									getCore().set_angle(getGraph(),
											    qv, lab.getText());
									updateGraph();
								}
								catch (QuantoCore.ConsoleError err) {
									errorDialog(err.getMessage());
								}
							}
						}
					});
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

				return new JLabel();
			}
			else {
				return new JLabel((String) value);
			}
		}

		/**
		 * Removes orphaned labels.
		 */
		public void cleanup() {
			synchronized (components) {
				for (Labeler l : components.values()) {
					viewer.remove(l);
				}
			}
			components = Collections.<QVertex, Labeler>synchronizedMap(
				new HashMap<QVertex, Labeler>());
		}
	}

	/**
	 * A graph mouse for doing most interactive graph operations.
	 *
	 */
	private class RWMouse extends PluggableGraphMouse
	{
		private GraphMousePlugin pickingMouse, edgeMouse, bangBoxMouse;
		private boolean pickingMouseActive, edgeMouseActive, bangBoxMouseActive;

		public RWMouse() {
			int mask = InputEvent.CTRL_MASK;
			if (QuantoApp.isMac) {
				mask = InputEvent.META_MASK;
			}

			add(new ScalingGraphMousePlugin(new ViewScalingControl(), 0));
			add(new ViewTranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | mask));
			add(new AddEdgeGraphMousePlugin<QVertex, QEdge>(
				viewer,
				InteractiveGraphView.this,
				InputEvent.BUTTON1_MASK | InputEvent.ALT_MASK));
			pickingMouse = new ConstrainedPickingGraphMousePlugin<QVertex, QEdge>(20, 20)
			{
				public void mouseEntered(MouseEvent e) {
				}

				public void mouseExited(MouseEvent e) {
				}
			};
			edgeMouse = new AddEdgeGraphMousePlugin<QVertex, QEdge>(
				viewer,
				InteractiveGraphView.this,
				InputEvent.BUTTON1_MASK);
			bangBoxMouse = new BangBoxGraphMousePlugin(InteractiveGraphView.this);
			setPickingMouse();
		}

		public void clearMouse() {
			edgeMouseActive = false;
			remove(edgeMouse);

			pickingMouseActive = false;
			remove(pickingMouse);

			bangBoxMouseActive = false;
			remove(bangBoxMouse);
		}

		public void setPickingMouse() {
			clearMouse();
			pickingMouseActive = true;
			add(pickingMouse);
			InteractiveGraphView.this.repaint();
		}

		public void setEdgeMouse() {
			clearMouse();
			edgeMouseActive = true;
			add(edgeMouse);
			InteractiveGraphView.this.repaint();
		}

		public void setBangBoxMouse() {
			clearMouse();
			bangBoxMouseActive = true;
			add(bangBoxMouse);
			InteractiveGraphView.this.repaint();
		}

		public boolean isPickingMouse() {
			return pickingMouseActive;
		}

		public boolean isEdgeMouse() {
			return edgeMouseActive;
		}

		public boolean isBangBoxMouse() {
			return bangBoxMouseActive;
		}

		public ItemListener getItemListener() {
			return new ItemListener()
			{
				public void itemStateChanged(ItemEvent e) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						if (e.getSource() == rbEdgeMode) {
							setEdgeMouse();
						}
						else if (e.getSource() == rbBangBoxMode) {
							setBangBoxMouse();
						}
						else {
							setPickingMouse();
						}
					}
				}
			};
		}
	}

	public InteractiveGraphView(QuantoCore core, QuantoGraph g) {
		this(core, g, new Dimension(800, 600));
	}

	public InteractiveGraphView(QuantoCore core, QuantoGraph g, Dimension size) {
		super(g, size);
		this.core = core;
		viewer.setLayoutSmoothingEnabled(true);

		pickedBangBoxState = new MultiPickedState<BangBox>();
		viewer.setPickedBangBoxState(pickedBangBoxState);

		//JLabel lab = new JLabel("test");
		//add(lab);
		Relaxer r = viewer.getModel().getRelaxer();
		if (r != null) {
			r.setSleepTime(10);
		}

		graphMouse = new RWMouse();
		viewer.setGraphMouse(graphMouse);
		menus = new ArrayList<JMenu>();
		buildMenus();

		viewer.addPreRenderPaintable(new VisualizationServer.Paintable()
		{
			public void paint(Graphics g) {
				Color old = g.getColor();
				g.setColor(Color.red);
				if (graphMouse.isEdgeMouse()) {
					g.drawString("EDGE MODE", 5, 15);
				}
				else if (graphMouse.isBangBoxMouse()) {
					g.drawString("!-BOX MODE", 5, 15);
				}
				g.setColor(old);
			}

			public boolean useTransform() {
				return false;
			}
		});

		viewer.addMouseListener(new MouseAdapter()
		{
			public void mousePressed(MouseEvent e) {
				InteractiveGraphView.this.grabFocus();
				super.mousePressed(e);
			}
		});

		viewer.addKeyListener(new KeyAdapter()
		{
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
								rbPickingMode.setSelected(true);
							}
							else {
								rbEdgeMode.setSelected(true);
							}
							break;
						case KeyEvent.VK_N:
							if (graphMouse.isBangBoxMouse()) {
								rbPickingMode.setSelected(true);
							}
							else {
								rbBangBoxMode.setSelected(true);
							}
							break;
						case KeyEvent.VK_SPACE:
							showRewrites();
							break;
					}
				}
			}
		});

		viewer.getRenderContext().setVertexStrokeTransformer(
			new Transformer<QVertex, Stroke>()
			{
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
			new Transformer<QVertex, Paint>()
			{
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
	}

	private void errorDialog(String msg) {
		JOptionPane.showMessageDialog(this,
					      msg,
					      "Console Error",
					      JOptionPane.ERROR_MESSAGE);
	}

	private void infoDialog(String msg) {
		JOptionPane.showMessageDialog(this, msg);
	}

	public static String titleOfGraph(String name) {
		return "graph (" + name + ")";
	}

//	public String getTitle() {
//		return InteractiveGraphView.titleOfGraph(getGraph().getName());
//	}
	private void buildMenus() {
		int commandMask;
		if (QuantoApp.isMac) {
			commandMask = Event.META_MASK;
		}
		else {
			commandMask = Event.CTRL_MASK;
		}

		// Save Graph
		file_saveGraph = new JMenuItem("Save Graph", KeyEvent.VK_S);
		file_saveGraph.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e) {
				saveGraph();
			}
		});
		file_saveGraph.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, commandMask));

		// Save Graph As
		file_saveGraphAs = new JMenuItem("Save Graph As...", KeyEvent.VK_A);
		file_saveGraphAs.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e) {
				saveGraphAs();
			}
		});
		file_saveGraphAs.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, commandMask | Event.SHIFT_MASK));

		JMenu graphMenu = new JMenu("Graph");
		graphMenu.setMnemonic(KeyEvent.VK_G);

		JMenuItem item;

		item = new JMenuItem("Export to PDF", KeyEvent.VK_P);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				GraphVisualizationViewer view = new GraphVisualizationViewer(getGraph());
				byte[] gr = view.exportPdf();

				System.out.printf("Got %d bytes of data.\n", gr.length);
				try {
					BufferedOutputStream file = new BufferedOutputStream(
						new FileOutputStream("/Users/aleks/itexttest.pdf"));
					file.write(gr);
					file.close();
				}
				catch (IOException exp) {
					throw new ConsoleError(exp.getMessage());
				}
			}
		});
		graphMenu.add(item);

		graphMenu.addSeparator();

		ButtonGroup mouseModeGroup = new ButtonGroup();
		rbPickingMode = new JRadioButtonMenuItem("Select Mode");
		rbPickingMode.setMnemonic(KeyEvent.VK_T);
		rbPickingMode.addItemListener(graphMouse.getItemListener());
		mouseModeGroup.add(rbPickingMode);
		graphMenu.add(rbPickingMode);


		rbEdgeMode = new JRadioButtonMenuItem("Edge Mode");
		rbEdgeMode.setMnemonic(KeyEvent.VK_E);
		rbEdgeMode.addItemListener(graphMouse.getItemListener());
		mouseModeGroup.add(rbEdgeMode);
		graphMenu.add(rbEdgeMode);

		rbBangBoxMode = new JRadioButtonMenuItem("Bang Box Mode");
		rbBangBoxMode.setMnemonic(KeyEvent.VK_B);
		rbBangBoxMode.addItemListener(graphMouse.getItemListener());
		mouseModeGroup.add(rbBangBoxMode);
		graphMenu.add(rbBangBoxMode);

		graphMenu.addSeparator();

//		cbBangBoxMode = new JCheckBoxMenuItem("Add Edge Mode");
//		cbBangBoxMode.setMnemonic(KeyEvent.VK_E);
//		cbBangBoxMode.addItemListener(graphMouse.getItemListener());
//		cbBangBoxMode.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, commandMask));
//		graphMenu.add(cbBangBoxMode);

		item = new JMenuItem("Latex to clipboard", KeyEvent.VK_L);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				String tikz = TikzOutput.generate(getGraph(), viewer.getGraphLayout());
				Clipboard cb = Toolkit.getDefaultToolkit().getSystemClipboard();
				StringSelection data = new StringSelection(tikz);
				cb.setContents(data, data);
			}
		});
		graphMenu.add(item);

		JMenu graphAddMenu = new JMenu("Add");
		item = new JMenuItem("Red Vertex", KeyEvent.VK_R);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.RED);
			}
		});
		graphAddMenu.add(item);

		item = new JMenuItem("Green Vertex", KeyEvent.VK_G);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.GREEN);
			}
		});
		graphAddMenu.add(item);

		item = new JMenuItem("Boundary Vertex", KeyEvent.VK_B);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.BOUNDARY);
			}
		});
		graphAddMenu.add(item);

		item = new JMenuItem("Hadamard Gate", KeyEvent.VK_H);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.HADAMARD);
			}
		});
		graphAddMenu.add(item);

		graphMenu.add(graphAddMenu);

		item = new JMenuItem("Show Rewrites", KeyEvent.VK_R);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				showRewrites();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, commandMask | Event.ALT_MASK));
		graphMenu.add(item);

		item = new JMenuItem("Normalise", KeyEvent.VK_N);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				rewriteForever();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, commandMask | Event.ALT_MASK));
		graphMenu.add(item);

		item = new JMenuItem("Fast Normalise", KeyEvent.VK_F);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().fastNormalise(getGraph());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, commandMask | Event.ALT_MASK));
		graphMenu.add(item);

		item = new JMenuItem("Abort", KeyEvent.VK_A);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				if (rewriter != null) {
					rewriter.interrupt();
					rewriter = null;
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD, commandMask));
		graphMenu.add(item);

		item = new JMenuItem("Select All Vertices", KeyEvent.VK_S);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				synchronized (getGraph()) {
					for (QVertex v : getGraph().getVertices()) {
						viewer.getPickedVertexState().pick(v, true);
					}
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, commandMask));
		graphMenu.add(item);

		item = new JMenuItem("Deselect All Vertices", KeyEvent.VK_D);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				viewer.getPickedVertexState().clear();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, commandMask | KeyEvent.SHIFT_MASK));
		graphMenu.add(item);

		item = new JMenuItem("Lock Vertices", KeyEvent.VK_L);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				viewer.lock(viewer.getPickedVertexState().getPicked());
				repaint();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, commandMask));
		graphMenu.add(item);

		item = new JMenuItem("Unlock Vertices", KeyEvent.VK_N);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				viewer.unlock(viewer.getPickedVertexState().getPicked());
				repaint();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
							   KeyEvent.SHIFT_MASK | commandMask));
		graphMenu.add(item);

		item = new JMenuItem("Flip Vertex Colour", KeyEvent.VK_F);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().flip_vertices(getGraph(),
							viewer.getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, commandMask));
		graphMenu.add(item);

		// define submenu for bang boxes
		JMenu bangMenu = new JMenu("Bang boxes");

		item = new JMenuItem("Bang Vertices", KeyEvent.VK_B);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				// this is not the real bang box, but we just need the name
				BangBox bb = new BangBox(getCore().add_bang(getGraph()));
				getCore().bang_vertices(getGraph(), bb,
							viewer.getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1, commandMask));
		bangMenu.add(item);

		item = new JMenuItem("Unbang Vertices", KeyEvent.VK_U);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().unbang_vertices(getGraph(),
							  viewer.getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1,
							   commandMask | KeyEvent.SHIFT_MASK));
		bangMenu.add(item);
		graphMenu.add(bangMenu);


		item = new JMenuItem("Drop Box", KeyEvent.VK_P);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().bbox_drop(getGraph(),
						    getPickedBangBoxState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, KeyEvent.SHIFT_MASK));
		bangMenu.add(item);
		graphMenu.add(bangMenu);


		item = new JMenuItem("Kill Box", KeyEvent.VK_K);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().bbox_kill(getGraph(),
						    getPickedBangBoxState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, commandMask | KeyEvent.SHIFT_MASK));
		bangMenu.add(item);
		graphMenu.add(bangMenu);

		item = new JMenuItem("Duplicate Box", KeyEvent.VK_D);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().bbox_duplicate(getGraph(),
							 getPickedBangBoxState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, commandMask));
		bangMenu.add(item);
		graphMenu.add(bangMenu);

		menus.add(graphMenu);

		JMenu editMenu = new JMenu("Edit");
		editMenu.setMnemonic(KeyEvent.VK_E);
		item = new JMenuItem("Undo", KeyEvent.VK_U);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().undo(getGraph());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, commandMask));
		editMenu.add(item);

		item = new JMenuItem("Redo", KeyEvent.VK_R);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().redo(getGraph());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_Z, commandMask | KeyEvent.SHIFT_MASK));
		editMenu.add(item);

		item = new JMenuItem("Cut", KeyEvent.VK_T);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				Set<QVertex> picked = viewer.getPickedVertexState().getPicked();
				if (!picked.isEmpty()) {
					getCore().copy_subgraph(getGraph(), "__clip__", picked);
					getCore().delete_vertices(getGraph(), picked);
					updateGraph();
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_X, commandMask));
		editMenu.add(item);

		item = new JMenuItem("Copy", KeyEvent.VK_C);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				Set<QVertex> picked = viewer.getPickedVertexState().getPicked();
				if (!picked.isEmpty()) {
					getCore().copy_subgraph(getGraph(), "__clip__", picked);
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_C, commandMask));
		editMenu.add(item);

		item = new JMenuItem("Paste", KeyEvent.VK_P);
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().insert_graph(getGraph(), "__clip__");
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_V, commandMask));
		editMenu.add(item);

		menus.add(editMenu);

		JMenu hilbMenu = new JMenu("Hilbert Space");
		hilbMenu.setMnemonic(KeyEvent.VK_B);

		item = new JMenuItem("Dump term as text");
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				outputToTextView(getCore().hilb(getGraph(), "plain"));
			}
		});
		hilbMenu.add(item);

		item = new JMenuItem("Dump term as Mathematica");
		item.addActionListener(new QuantoActionListener(this)
		{
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				outputToTextView(getCore().hilb(getGraph(), "mathematica"));
			}
		});
		hilbMenu.add(item);

		menus.add(hilbMenu);
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
			getCore().attach_rewrites(getGraph(), viewer.getPickedVertexState().getPicked());
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

		if (file_saveGraph != null && getGraph().getFileName() != null && !getGraph().isSaved()) {
			file_saveGraph.setEnabled(true);
		}

		viewer.update();
	}

	public void outputToTextView(String text) {
		TextView tview = new TextView(text);
		QuantoApp app = QuantoApp.getInstance();
		app.addView(app.getViewName(this) + "-output", tview);
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

	public void rewriteForever() {
		rewriter = new Thread()
		{
			private void attach() {
				try {
					getCore().attach_one_rewrite(
						getGraph(),
						getGraph().getVertices());
				}
				catch (QuantoCore.ConsoleError e) {
					errorDialog(e.getMessage());
				}
			}

			public void run() {
				attach();
				List<Rewrite> rws = getRewrites();
				int count = 0;
				Random r = new Random();
				int rw = 0;
				while (rws.size() > 0
					&& Thread.currentThread() == rewriter) {
					rw = r.nextInt(rws.size());
					highlightSubgraph(rws.get(rw).getLhs());
					try {
						sleep(1500);
						clearHighlight();
						applyRewrite(rw);
						++count;
						attach();
						rws = getRewrites();
					}
					catch (InterruptedException e) {
						clearHighlight();
						break;
					}
				}


				final int finalCount = count;
				try {
					SwingUtilities.invokeAndWait(new Runnable()
					{
						public void run() {
							infoDialog("Applied "
								+ finalCount
								+ " rewrites.");
						}
					});
				}
				catch (InterruptedException e) {
					e.printStackTrace();
				}
				catch (InvocationTargetException e) {
					e.printStackTrace();
				}
			}
		};
		rewriter.start();
	}

	private class SubgraphHighlighter
		implements VisualizationServer.Paintable
	{
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

	public void saveGraphAs() {
		int retVal = QuantoApp.getInstance().getFileChooser().showSaveDialog(this);
		if (retVal == JFileChooser.APPROVE_OPTION) {
			try {
				File f = QuantoApp.getInstance().getFileChooser().getSelectedFile();
				String filename = f.getCanonicalPath().replaceAll("\\n|\\r", "");
				core.save_graph(getGraph(), filename);
				getGraph().setFileName(filename);
				getGraph().setSaved(true);
				QuantoApp.getInstance().renameView(this, f.getName());
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

	public void viewFocus(ViewPort vp) {
//		System.out.printf("Adding (%d) menus (%s).\n", menus.size(), getGraph().getName());
		QuantoApp.MainMenu mm = vp.getMainMenu();
		for (JMenu menu : menus) {
			mm.add(menu);
		}
		mm.insertAfter(mm.fileMenu, mm.file_openGraph, file_saveGraph);
		mm.insertAfter(mm.fileMenu, file_saveGraph, file_saveGraphAs);
		mm.file_closeView.setEnabled(true);
		mm.revalidate();
		mm.repaint();
		grabFocus();
//		setBorder(BorderFactory.createLineBorder(Color.blue, 1));
	}

	public void viewUnfocus(ViewPort vp) {
//		System.out.printf("Removin my stuff (%s).\n", getGraph().getName());
		QuantoApp.MainMenu mm = vp.getMainMenu();
		for (JMenu menu : menus) {
			mm.remove(menu);
		}
		mm.fileMenu.remove(file_saveGraph);
		mm.fileMenu.remove(file_saveGraphAs);
		mm.repaint();
//		setBorder(null);
	}

	public byte[] exportPdf() {
		System.out.println(
			"WARNING: exportPdf() in InteractGraphView may have funky output.\n"
			+ "Use GraphView instead.");
		return viewer.exportPdf();
	}

	public void viewKillNoPrompt() {
		// TODO: unload graph
	}

	public boolean viewKill(ViewPort vp) {
		boolean kill = false;
		if (getGraph().isSaved()) {
			kill = true;
		}
		else {
			String msg = "Graph '" + getGraph().getName() + "' is unsaved. Close anyway?";
			kill = (JOptionPane.showConfirmDialog(this, msg,
							      "Unsaved changes", JOptionPane.YES_NO_OPTION) == 0);
		}

		if (kill == true) {
			viewKillNoPrompt();
		}
		return kill;
	}

	public PickedState<BangBox> getPickedBangBoxState() {
		return pickedBangBoxState;
	}

	public boolean isSaved() {
		return getGraph().isSaved();
	}
}
