package quanto.gui;

import quanto.core.QuantoGraph;
import edu.uci.ics.jung.contrib.ViewZoomScrollPane;
import java.awt.Dimension;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.event.ChangeListener;

public class GraphView extends JPanel
{
	private static final long serialVersionUID = -1915610684250038897L;
	protected GraphVisualizationViewer viewer;
	private boolean hasScrollbars;

	public GraphView(QuantoGraph g) {
		this(g, new Dimension(800, 600));
	}

	public GraphView(QuantoGraph g, boolean scrollable) {
		this(g, new Dimension(800, 600), scrollable);
	}

	public GraphView(QuantoGraph graph, Dimension size) {
		this(graph, size, true);
	}

	public GraphView(QuantoGraph graph, Dimension size, boolean scrollable) {
		super(new BorderLayout());
		setPreferredSize(size);
		hasScrollbars = scrollable;

		viewer = new GraphVisualizationViewer(graph);
		if (scrollable)
			add(new ViewZoomScrollPane(viewer), BorderLayout.CENTER);
		else
			add(viewer, BorderLayout.CENTER);
	}

	public boolean hasScrollbars() {
		return hasScrollbars;
	}

	public void setHasScrollbars(boolean hasScrollbars) {
		if (this.hasScrollbars != hasScrollbars)
		{
			if (hasScrollbars)
				add(new ViewZoomScrollPane(viewer), BorderLayout.CENTER);
			else
				add(viewer, BorderLayout.CENTER);
			this.hasScrollbars = hasScrollbars;
		}
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
}
