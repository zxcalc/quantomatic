package quanto.gui;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.n3.nanoxml.*;
import org.apache.commons.collections15.Transformer;
import quanto.gui.QuantoCore.ConsoleError;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.contrib.SmoothLayoutDecorator;
import edu.uci.ics.jung.graph.util.Pair;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;

public class InteractiveGraphView extends GraphView
implements AddEdgeGraphMousePlugin.Adder<QVertex>, InteractiveView {
	private static final long serialVersionUID = 7196565776978339937L;
	private QuantoCore core;
	private RWMouse graphMouse;
	protected List<JMenu> menus;
	private InteractiveView.Holder viewHolder;
	private JMenuItem saveGraphMenuItem = null; // the view needs to manage when this menu is alive or not.
	
	/**
	 * Generic action listener that reports errors to a dialog box and gives
	 * actions access to the frame, console, and core.
	 */
	public abstract class QVListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			try {
				wrappedAction(e);
			} catch (QuantoCore.ConsoleError err) {
				JOptionPane.showMessageDialog(
						InteractiveGraphView.this,
						err.getMessage(),
						"Console Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
		
		public abstract void wrappedAction(ActionEvent e) throws QuantoCore.ConsoleError;
	}
	
	private class QVertexLabeler implements VertexLabelRenderer {
		Map<QVertex,Labeler> components;
		
		public QVertexLabeler () {
			components = Collections.<QVertex, Labeler>synchronizedMap(
							new HashMap<QVertex, Labeler>());
		}
		
		public <T> Component getVertexLabelRendererComponent(JComponent vv,
				Object value, Font font, boolean isSelected, T vertex) {
			if (vertex instanceof QVertex && ((QVertex)vertex).isAngleVertex()) {
				Point2D screen = getRenderContext().
					getMultiLayerTransformer().transform(
						getGraphLayout().transform((QVertex)vertex));
				
				// lazily create the labeler
				Labeler angleLabeler = components.get(vertex);
				if (angleLabeler == null) {
					angleLabeler = new Labeler("");
					components.put((QVertex)vertex,angleLabeler);
					InteractiveGraphView.this.add(angleLabeler);
					final QVertex qv = (QVertex)vertex;
					if (qv.getColor().equals(Color.red)) {
						angleLabeler.setColor(new Color(255,170,170));
					} else {
						angleLabeler.setColor(new Color(150,255,150));
					}
					
					String angle = ((QVertex)vertex).getAngle();
					Rectangle rect = new Rectangle(angleLabeler.getPreferredSize());
					Point loc = new Point((int)(screen.getX()-rect.getCenterX()),
							  (int)screen.getY()+10);
					rect.setLocation(loc);
					
					if (!angleLabeler.getText().equals(angle)) angleLabeler.setText(angle);
					if (!angleLabeler.getBounds().equals(rect)) angleLabeler.setBounds(rect);
					
					angleLabeler.addChangeListener(new ChangeListener() {
						public void stateChanged(ChangeEvent e) {
							Labeler lab = (Labeler)e.getSource();
							if (qv != null) {
								try {
									getCore().set_angle(getGraph(),
											qv, lab.getText());
									updateGraph();
								} catch (QuantoCore.ConsoleError err) {
									errorDialog(err.getMessage());
								}
							}
						}
					});
				}
				
				
				
				String angle = ((QVertex)vertex).getAngle();
				Rectangle rect = new Rectangle(angleLabeler.getPreferredSize());
				Point loc = new Point((int)(screen.getX()-rect.getCenterX()),
						  (int)screen.getY()+10);
				rect.setLocation(loc);
				
				if (!angleLabeler.getText().equals(angle)) angleLabeler.setText(angle);
				if (!angleLabeler.getBounds().equals(rect)) {
					angleLabeler.setBounds(rect);
				}
						
				return new JLabel();
			} else {
				return new JLabel((String)value);
			}
		}
		
		/**
		 * Removes orphaned labels.
		 */
		public void cleanup() {
			synchronized (components) {
				for (Labeler l : components.values())
					InteractiveGraphView.this.remove(l);
			}
			components = Collections.<QVertex, Labeler>synchronizedMap(
							new HashMap<QVertex, Labeler>());
		}
	}
	
	/**
	 * A graph mouse for doing most interactive graph operations.
	 *
	 */
	private class RWMouse extends PluggableGraphMouse {
		private GraphMousePlugin pickingMouse, edgeMouse;
		private boolean pickingMouseSelected;
		public RWMouse() {
			int mask = InputEvent.CTRL_MASK;
			if (QuantoFrame.isMac) mask = InputEvent.META_MASK;
			
			add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, 1.1f, 0.909f));
			add(new TranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | mask));
			pickingMouse = new PickingGraphMousePlugin<QVertex,QEdge>();
			edgeMouse = new AddEdgeGraphMousePlugin<QVertex,QEdge>(
							InteractiveGraphView.this,
							InteractiveGraphView.this,
							InputEvent.BUTTON1_MASK);
			setPickingMouse();
		}
		
		public void setPickingMouse() {
			pickingMouseSelected = true;
			remove(edgeMouse);
			add(pickingMouse);
		}
		
		public void setEdgeMouse() {
			pickingMouseSelected = false;
			remove(pickingMouse);
			add(edgeMouse);
		}
		
		public boolean isPickingMouse() {
			return pickingMouseSelected;
		}
		
		public boolean isEdgeMouse() {
			return !pickingMouseSelected;
		}
		
		public ItemListener getItemListener() {
			return new ItemListener () {
				public void itemStateChanged(ItemEvent e) {
					if (e.getStateChange() == ItemEvent.SELECTED) {
						setEdgeMouse();
					} else {
						setPickingMouse();
					}
				}
			};
		}
	}
	
	public InteractiveGraphView(QuantoCore core, QuantoGraph g) {
		this(core, g, new Dimension(800,600));
	}
	
	public InteractiveGraphView(QuantoCore core, QuantoGraph g, Dimension size) {
		this(core, g, size, null);
	}
	
	public InteractiveGraphView(QuantoCore core, QuantoGraph g, Dimension size, JMenuItem saveItem) {
		super(g, size);
		this.core = core;
		this.viewHolder = null;
		setGraphLayout(new SmoothLayoutDecorator<QVertex,QEdge>(getQuantoLayout()));
		setLayout(null);
		
		//JLabel lab = new JLabel("test");
		//add(lab);
		Relaxer r = getModel().getRelaxer();
		if (r!= null) r.setSleepTime(10);
		
		graphMouse = new RWMouse();
		setGraphMouse(graphMouse);
		menus = new ArrayList<JMenu>();
		buildMenus();
		
		addPreRenderPaintable(new VisualizationServer.Paintable() {
			public void paint(Graphics g) {
				Color old = g.getColor();
				g.setColor(Color.red);
				if (graphMouse.isEdgeMouse())
					g.drawString("EDGE MODE", 5, 15);
				g.setColor(old);
			}

			public boolean useTransform() {return false;}
		});
		
		addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				// this listener only handles un-modified keys
				if (e.getModifiers() != 0) return;
				
				int delete = (QuantoFrame.isMac) ? KeyEvent.VK_BACK_SPACE : KeyEvent.VK_DELETE;
				if (e.getKeyCode() == delete) {
					try {
						getCore().delete_edges(
								getGraph(), getPickedEdgeState().getPicked());
						getCore().delete_vertices(
								getGraph(), getPickedVertexState().getPicked());
						updateGraph();
					
					} catch (QuantoCore.ConsoleError err) {
						errorDialog(err.getMessage());
					} finally {
						// if null things are in the picked state, weird stuff
						// could happen.
						getPickedEdgeState().clear();
						getPickedVertexState().clear();
					}
				} else {
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
					}
				}
			}
		});
		
        getRenderContext().setVertexStrokeTransformer(
        		new Transformer<QVertex,Stroke>() {
					public Stroke transform(QVertex v) {
						if (getPickedVertexState().isPicked(v)) 
							 return new BasicStroke(3);
						else return new BasicStroke(1);
					}
        		});
        getRenderContext().setVertexDrawPaintTransformer(
        		new Transformer<QVertex, Paint>() {
					public Paint transform(QVertex v) {
						if (getPickedVertexState().isPicked(v)) 
							 return Color.blue;
						else return Color.black;
					}
        		});
        
        
        getRenderContext().setVertexLabelRenderer(new QVertexLabeler());
        
        // a bit hackish
        this.saveGraphMenuItem = saveItem;
	}
	
	private void errorDialog(String msg) {
		JOptionPane.showMessageDialog(this,
				msg,
				"Console Error",
				JOptionPane.ERROR_MESSAGE);
	}
	
	public static String titleOfGraph(String name) {
		return "graph (" + name + ")";
	}
	
	public String getTitle() {
		return InteractiveGraphView.titleOfGraph(getGraph().getName());
	}
	
	private void buildMenus() {
		int commandMask;
	    if (QuantoFrame.isMac) commandMask = Event.META_MASK;
	    else commandMask = Event.CTRL_MASK;
		
	    JMenu graphMenu = new JMenu("Graph");
		graphMenu.setMnemonic(KeyEvent.VK_G);
		
		JMenuItem item;
		
		JCheckBoxMenuItem cbItem = new JCheckBoxMenuItem("Add Edge Mode");
		cbItem.setMnemonic(KeyEvent.VK_E);
		cbItem.addItemListener(graphMouse.getItemListener());
		cbItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, commandMask));
		graphMenu.add(cbItem);
		
		JMenu graphAddMenu = new JMenu("Add");
		item = new JMenuItem("Red Vertex", KeyEvent.VK_R);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.RED);
			}
		});
		graphAddMenu.add(item);
		
		item = new JMenuItem("Green Vertex", KeyEvent.VK_G);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.GREEN);
			}
		});
		graphAddMenu.add(item);
		
		item = new JMenuItem("Boundary Vertex", KeyEvent.VK_B);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.BOUNDARY);
			}
		});
		graphAddMenu.add(item);
		
		item = new JMenuItem("Hadamard Gate", KeyEvent.VK_H);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				addVertex(QVertex.Type.HADAMARD);
			}
		});
		graphAddMenu.add(item);
		
		graphMenu.add(graphAddMenu);
		
		item = new JMenuItem("Show Rewrites", KeyEvent.VK_R);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().attach_rewrites(getGraph(), getPickedVertexState().getPicked());
				JFrame rewrites = new RewriteViewer(InteractiveGraphView.this);
				rewrites.setVisible(true);
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, commandMask | Event.ALT_MASK));
		graphMenu.add(item);
		
		item = new JMenuItem("Select All Vertices", KeyEvent.VK_S);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				synchronized (getGraph()) {
					for (QVertex v : getGraph().getVertices()) {
						getPickedVertexState().pick(v, true);
					}
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, commandMask));
		graphMenu.add(item);
		
		item = new JMenuItem("Deselect All Vertices", KeyEvent.VK_D);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getPickedVertexState().clear();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, commandMask));
		graphMenu.add(item);
		
		item = new JMenuItem("Flip Vertex Colour", KeyEvent.VK_F);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().flip_vertices(getGraph(),
					getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, commandMask));
		graphMenu.add(item);
		
		item = new JMenuItem("Bang Vertices", KeyEvent.VK_B);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				// this is not the real bang box, but we just need the name
				BangBox bb = new BangBox(getCore().add_bang(getGraph()));
				getCore().bang_vertices(getGraph(), bb, 
						getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1, commandMask));
		graphMenu.add(item);
		
		item = new JMenuItem("Unbang Vertices", KeyEvent.VK_U);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().unbang_vertices(getGraph(),
						getPickedVertexState().getPicked());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_1,
				commandMask | KeyEvent.SHIFT_MASK));
		graphMenu.add(item);
		
		menus.add(graphMenu);
		
		JMenu editMenu = new JMenu("Edit");
		editMenu.setMnemonic(KeyEvent.VK_E);
		item = new JMenuItem("Undo", KeyEvent.VK_U);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().undo(getGraph());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, commandMask));
		editMenu.add(item);
		
		item = new JMenuItem("Redo", KeyEvent.VK_R);
		item.addActionListener(new QVListener() {
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
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				Set<QVertex> picked = getPickedVertexState().getPicked();
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
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				Set<QVertex> picked = getPickedVertexState().getPicked();
				if (!picked.isEmpty())
					getCore().copy_subgraph(getGraph(), "__clip__", picked);
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_C, commandMask));
		editMenu.add(item);
		
		item = new JMenuItem("Paste", KeyEvent.VK_P);
		item.addActionListener(new QVListener() {
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
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				outputToTextView(getCore().hilb(getGraph(), "plain"));
			}
		});
		hilbMenu.add(item);
		
		item = new JMenuItem("Dump term as Mathematica");
		item.addActionListener(new QVListener() {
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
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	
	public void addVertex(QVertex.Type type) {
		try {
			getCore().add_vertex(getGraph(), type);
			updateGraph();
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		}
	}
	
	public List<JMenu> getMenus() {
		return menus;
	}
	
	
	public void updateGraph() throws QuantoCore.ConsoleError {
		String xml = getCore().graph_xml(getGraph());
		// ignore the first line
		xml = xml.replaceFirst("^.*", "");
		getGraph().fromXml(xml);
		getGraphLayout().initialize();
		
		((ChangeEventSupport)getGraphLayout()).fireStateChanged();
		
		Relaxer relaxer = getModel().getRelaxer();
		if (relaxer != null) relaxer.relax();
		
		// clean up un-needed labels:
		((QVertexLabeler)getRenderContext().getVertexLabelRenderer()).cleanup();
		
		// re-validate the picked state
		QVertex[] oldPicked = 
			getPickedVertexState().getPicked().toArray(
				new QVertex[getPickedVertexState().getPicked().size()]);
		getPickedVertexState().clear();
		Map<String,QVertex> vm = getGraph().getVertexMap();
		for (QVertex v : oldPicked) {
			QVertex new_v = vm.get(v.getName());
			if (new_v != null) getPickedVertexState().pick(new_v, true);
		}
		
		if(saveGraphMenuItem != null && getGraph().getFileName() != null && !getGraph().isSaved()) 
			saveGraphMenuItem.setEnabled(true);
		
		repaint();
	}
	
	public void outputToTextView(String text) {
		TextView tview = new TextView(text);
		if (viewHolder != null) {
			viewHolder.addView(tview);
		} else {
			JFrame out = new JFrame();
			out.setTitle(tview.getTitle());
			out.getContentPane().add(tview);
			out.pack();
			out.setVisible(true);
		}
	}
	
	/**
	 * Gets the attached rewrites as a list of Pair<QuantoGraph>. Returns and empty
	 * list on console error.
	 * @return
	 */
	public List<Pair<QuantoGraph>> getRewrites() {
		List<Pair<QuantoGraph>> rewrites = new ArrayList<Pair<QuantoGraph>>();
		try {
			String xml = getCore().show_rewrites(getGraph());
			try {
				IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
				parser.setReader(StdXMLReader.stringReader(xml));
				IXMLElement root = (IXMLElement)parser.parse();
				for (Object obj : root.getChildrenNamed("rewrite")) {
					IXMLElement rw = (IXMLElement)obj;
					IXMLElement lhs = rw.getFirstChildNamed("lhs")
						.getFirstChildNamed("graph");
					IXMLElement rhs = rw.getFirstChildNamed("rhs")
						.getFirstChildNamed("graph");
					rewrites.add(new Pair<QuantoGraph>(
							new QuantoGraph().fromXml(lhs),
							new QuantoGraph().fromXml(rhs)
						));
					
				}
			} catch (XMLException e) {
				System.out.println("Error parsing XML.");
			} catch (ClassNotFoundException e) {
				throw new RuntimeException(e);
			} catch (InstantiationException e) {
				throw new RuntimeException(e);
			} catch (IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		} catch (QuantoCore.ConsoleError e) {
			errorDialog(e.getMessage());
		} catch (Exception e) {
			throw new RuntimeException(e); // all other exceptions are serious
		}
		
		return rewrites;
	}
	
	public void applyRewrite(int index) {
		try {
			getCore().apply_rewrite(getGraph(), index);
			updateGraph();
		} catch (ConsoleError e) {
			errorDialog("Error in rewrite. The graph probably changed "+
					"after this rewrite was attached.");
		}
	}

	private QuantoCore getCore() {
		return core;
	}
	
	public void gainFocus() {
		grabFocus();
		if(saveGraphMenuItem != null)
		{ 
			if(getGraph().getFileName() != null && !getGraph().isSaved()) 
				saveGraphMenuItem.setEnabled(true);
			else 
				saveGraphMenuItem.setEnabled(false);
		}
	}
	
	public void loseFocus() {
		
	}

	public void setViewHolder(InteractiveView.Holder viewHolder) {
		this.viewHolder = viewHolder;
	}

}
