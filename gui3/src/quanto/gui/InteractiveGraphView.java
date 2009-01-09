package quanto.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import net.n3.nanoxml.*;

import org.apache.commons.collections15.Transformer;

import quanto.gui.QuantoCore.ConsoleError;

import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.contrib.DotLayout;
import edu.uci.ics.jung.contrib.SmoothLayoutDecorator;
import edu.uci.ics.jung.graph.util.Pair;
import edu.uci.ics.jung.visualization.control.*;

public class InteractiveGraphView extends GraphView
implements AddEdgeGraphMousePlugin.Adder<QVertex>, InteractiveView {
	private static final long serialVersionUID = 7196565776978339937L;
	private QuantoCore core;
	private RWMouse graphMouse;
	protected List<JMenu> menus;
	private InteractiveView.Holder viewHolder;
	
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
	
	/**
	 * A graph mouse for doing most interactive graph operations.
	 *
	 */
	private class RWMouse extends PluggableGraphMouse {
		private GraphMousePlugin pickingMouse, edgeMouse;
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
			remove(edgeMouse);
			add(pickingMouse);
		}
		
		public void setEdgeMouse() {
			remove(pickingMouse);
			add(edgeMouse);
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
		super(g, size);
		this.core = core;
		this.viewHolder = null;
		setGraphLayout(new SmoothLayoutDecorator<QVertex,QEdge>(
				new DotLayout<QVertex,QEdge>(g,size)));
		Relaxer r = getModel().getRelaxer();
		if (r!= null) r.setSleepTime(4);
		
		graphMouse = new RWMouse();
		setGraphMouse(graphMouse);
		menus = new ArrayList<JMenu>();
		buildMenus();
		
		addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				int delete = (QuantoFrame.isMac) ? KeyEvent.VK_BACK_SPACE : KeyEvent.VK_DELETE;
				if (e.getKeyCode() == delete) {
					try {
						for (QEdge edge : getPickedEdgeState().getPicked())
							getCore().delete_edge(getGraph(), edge);
						for (QVertex vert : getPickedVertexState().getPicked())
							getCore().delete_vertex(getGraph(), vert);
						updateGraph();
					
					} catch (QuantoCore.ConsoleError err) {
						errorDialog(err.getMessage());
					} finally {
						// if null things are in the picked state, weird stuff
						// could happen.
						getPickedEdgeState().clear();
						getPickedVertexState().clear();
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
		
		item = new JMenuItem("Undo", KeyEvent.VK_U);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().undo(getGraph());
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, commandMask));
		graphMenu.add(item);
		
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
		graphMenu.add(item);
		
		JCheckBoxMenuItem cbItem = new JCheckBoxMenuItem("Add Edge Mode");
		cbItem.setMnemonic(KeyEvent.VK_E);
		cbItem.addItemListener(graphMouse.getItemListener());
		cbItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,0));
		graphMenu.add(cbItem);
		
		JMenu graphAddMenu = new JMenu("Add");
		item = new JMenuItem("Red Vertex", KeyEvent.VK_R);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getGraph(), QVertex.Type.RED);
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R, commandMask));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Green Vertex", KeyEvent.VK_G);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getGraph(), QVertex.Type.GREEN);
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, commandMask));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Boundary Vertex", KeyEvent.VK_B);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getGraph(), QVertex.Type.BOUNDARY);
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, commandMask));
		graphAddMenu.add(item);
		
		item = new JMenuItem("Hadamard Gate", KeyEvent.VK_M);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				getCore().add_vertex(getGraph(), QVertex.Type.HADAMARD);
				updateGraph();
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M, commandMask));
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
		
		item = new JMenuItem("Set Angle", KeyEvent.VK_A);
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				String def = "";
				if (getPickedVertexState().getPicked().size()>0) {
					QVertex fst = getPickedVertexState()
						.getPicked().iterator().next();
					def = fst.getAngle();
				}
				
				String angle = 
					JOptionPane.showInputDialog("Input a new angle:",def);
				
				if (angle != null) {
					for (QVertex v : getPickedVertexState().getPicked()) {
						if (v.getVertexType() != QVertex.Type.BOUNDARY)
							getCore().set_angle(getGraph(), v, angle);
					}
					updateGraph();
				}
			}
		});
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, commandMask | Event.ALT_MASK));
		graphMenu.add(item);
		
		menus.add(graphMenu);
		
		JMenu hilbMenu = new JMenu("Hilbert Space");
		hilbMenu.setMnemonic(KeyEvent.VK_B);
		
		item = new JMenuItem("Dump term as text");
		item.addActionListener(new QVListener() {
			@Override
			public void wrappedAction(ActionEvent e) throws ConsoleError {
				outputToTextView(getCore().hilb(getGraph(), "text"));
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
	
	public List<JMenu> getMenus() {
		return menus;
	}
	
	public void updateGraphFromXml(String xml) {
		getGraph().fromXml(xml);
		getGraphLayout().initialize();
	}
	
	public void updateGraph() throws QuantoCore.ConsoleError {
		String out = getCore().graph_xml(getGraph());
		// ignore the first line
		out = out.replaceFirst("^.*", "");
		updateGraphFromXml(out);
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

	public void setViewHolder(InteractiveView.Holder viewHolder) {
		this.viewHolder = viewHolder;
	}

}
