package quanto.gui;

import edu.uci.ics.jung.contrib.visualization.ViewZoomScrollPane;
import quanto.core.Theory;
import quanto.core.data.CoreGraph;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.*;

public class GraphView extends JPanel
{
	private static final long serialVersionUID = -1915610684250038897L;
	protected GraphVisualizationViewer viewer;
	private boolean hasScrollbars;

	public GraphView(Theory theory, CoreGraph g) {
		this(theory, g, new Dimension(800, 600));
	}

	public GraphView(Theory theory, CoreGraph g, boolean scrollable) {
		this(theory, g, new Dimension(800, 600), scrollable);
	}

	public GraphView(Theory theory, CoreGraph graph, Dimension size) {
		this(theory, graph, size, true);
	}

	public GraphView(Theory theory, CoreGraph graph, Dimension size, boolean scrollable) {
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

	public CoreGraph getGraph() {
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
