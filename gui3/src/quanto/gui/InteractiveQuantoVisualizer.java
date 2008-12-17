package quanto.gui;

import java.awt.event.InputEvent;

import javax.swing.JOptionPane;

import edu.uci.ics.jung.contrib.AddEdgeGraphMousePlugin;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin;
import edu.uci.ics.jung.visualization.control.PluggableGraphMouse;
import edu.uci.ics.jung.visualization.control.ScalingGraphMousePlugin;
import edu.uci.ics.jung.visualization.control.TranslatingGraphMousePlugin;

public class InteractiveQuantoVisualizer extends QuantoVisualizer
implements AddEdgeGraphMousePlugin.Adder<QVertex> {
	private static final long serialVersionUID = 7196565776978339937L;
	protected QuantoCore core;
	
	private class RWMouse extends PluggableGraphMouse {
		public RWMouse() {
			int mask = InputEvent.CTRL_MASK;
			if (QuantoFrame.isMac) mask = InputEvent.META_MASK;
			
			add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, 1.1f, 0.909f));
			add(new TranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | mask));
			add(new PickingGraphMousePlugin<QVertex,QEdge>());
			add(new AddEdgeGraphMousePlugin<QVertex,QEdge>(
					InteractiveQuantoVisualizer.this,
					InteractiveQuantoVisualizer.this,
					InputEvent.BUTTON1_MASK | InputEvent.ALT_MASK));
		}
	}
	
	public InteractiveQuantoVisualizer(QuantoCore core, QuantoGraph g) {
		super(g);
		this.core = core;
		setGraphMouse(new RWMouse());
	}
	
	public void addEdge(QVertex s, QVertex t) {
		try {
			core.add_edge(getGraph(), s, t);
			updateGraph();
		} catch (QuantoCore.ConsoleError e) {
			JOptionPane.showMessageDialog(this,
					e.getMessage(),
					"Console Error",
					JOptionPane.ERROR_MESSAGE);
		}
	}
	
	public void updateGraphFromXml(String xml) {
		getGraph().fromXml(xml);
		getGraphLayout().initialize();
	}
	
	public void updateGraph() throws QuantoCore.ConsoleError {
		String out = core.graph_xml(getGraph());
		// ignore the first line
		out = out.replaceFirst("^.*", "");
		updateGraphFromXml(out);
	}

}
