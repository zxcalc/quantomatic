package quanto.gui;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;

public class GraphContext {
	public QuantoGraph graph;
	public VisualizationViewer<QuantoVertex,String> vis;
	public Layout<QuantoVertex,String> layout;
	public GraphContext(QuantoGraph graph,
			VisualizationViewer<QuantoVertex, String> vis,
			Layout<QuantoVertex, String> layout) {
		this.graph = graph;
		this.vis = vis;
		this.layout = layout;
	}
}
