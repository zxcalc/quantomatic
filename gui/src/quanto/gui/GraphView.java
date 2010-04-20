package quanto.gui;

import edu.uci.ics.jung.contrib.ViewZoomScrollPane;
import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JPanel;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import java.awt.BorderLayout;
import javax.swing.event.ChangeListener;

public class GraphView extends JPanel
{
	private static final long serialVersionUID = -1915610684250038897L;
	protected GraphVisualizationViewer viewer;

	public GraphView(QuantoGraph g) {
		this(g, new Dimension(800, 600));
	}

	public GraphView(QuantoGraph graph, Dimension size) {
		super(new BorderLayout());
		setPreferredSize(size);

		viewer = new GraphVisualizationViewer(graph);
		add(new ViewZoomScrollPane(viewer), BorderLayout.CENTER);
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
	 * Compute a bounding box and scale such that the largest dimension fits within the
	 * view port.
	 */
	public void zoomToFit() {
		MutableTransformer mt = viewer.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);
		Rectangle2D gb = viewer.getGraphBounds();
		Dimension vb = getSize();
		double centerX = vb.getWidth() / 2.0;
		double centerY = vb.getHeight() / 2.0;
		mt.translate(
			centerX - gb.getCenterX(),
			centerY - gb.getCenterY());
		float scale = Math.min(
			(float) (vb.getWidth() / gb.getWidth()),
			(float) (vb.getHeight() / gb.getHeight()));
		if (scale < 1) {
			mt.scale(scale, scale, new Point2D.Double(centerX, centerY));
		}
	}
}
