package quanto.gui;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;

public class GraphContext {
	public QuantoGraph graph;
	public VisualizationViewer<QVertex,String> vis;
	public Layout<QVertex,String> layout;
	public GraphContext(QuantoGraph graph,
			VisualizationViewer<QVertex, String> vis,
			Layout<QVertex, String> layout) {
		this.graph = graph;
		this.vis = vis;
		this.layout = layout;
	}
}
