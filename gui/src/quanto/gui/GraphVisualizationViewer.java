/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import javax.swing.event.ChangeEvent;
import quanto.core.QBangBox;
import quanto.core.QVertex;
import quanto.core.QEdge;
import quanto.core.QGraph;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.contrib.BangBoxLayout;
import edu.uci.ics.jung.contrib.MixedShapeTransformer;
import edu.uci.ics.jung.contrib.SmoothLayoutDecorator;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.picking.MultiPickedState;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Shape;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Set;
import javax.swing.event.ChangeListener;
import org.apache.commons.collections15.Predicate;
import quanto.gui.graphhelpers.QVertexAngleLabeler;
import quanto.gui.graphhelpers.BackdropPaintable;
import quanto.gui.graphhelpers.BangBoxPaintable;
import quanto.gui.graphhelpers.QVertexColorTransformer;
import quanto.gui.graphhelpers.QVertexLabelTransformer;
import quanto.gui.graphhelpers.QVertexRenderer;
import quanto.gui.graphhelpers.QVertexShapeTransformer;

/**
 *
 * @author alex
 */
public class GraphVisualizationViewer
       extends VisualizationViewer<QVertex, QEdge>
{
	private static final long serialVersionUID = -1723894723956293847L;
	private QGraph graph;
	private BangBoxPaintable bangBoxPainter;
	private BackdropPaintable boundsPaint;
	private boolean boundsPaintingEnabled = false;
	private BangBoxLayout<QVertex, QEdge, QBangBox> layout;
	private SmoothLayoutDecorator<QVertex, QEdge> smoothLayout;
	/**
	 * Holds the state of which bang boxes of the graph are currently
	 * "picked"
	 */
	protected PickedState<QBangBox> pickedBangBoxState;


	public GraphVisualizationViewer(QGraph graph) {
		this(QuantoApp.useExperimentalLayout ? new JavaQuantoDotLayout(graph) : new QuantoDotLayout(graph));
	}

	public GraphVisualizationViewer(BangBoxLayout<QVertex, QEdge, QBangBox> layout) {
		super(layout);

		this.layout = layout;

		if (!(layout.getGraph() instanceof QGraph)) {
			throw new IllegalArgumentException("Only QuantoGraphs are supported");
		}
		this.graph = (QGraph) layout.getGraph();

		this.bangBoxPainter = new BangBoxPaintable(layout, graph, this);

		layout.initialize();
		setBackground(new Color(0.97f, 0.97f, 0.97f));

		setupRendering();

		setPickedBangBoxState(new MultiPickedState<QBangBox>());
		setPreferredSize(calculateGraphSize());

		graph.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				relayout();
			}
		});
	}

	private void  setupRendering() {
		getRenderContext().setParallelEdgeIndexFunction(
			BalancedEdgeIndexFunction.<QVertex, QEdge>getInstance());

		getRenderContext().setEdgeShapeTransformer(
			new MixedShapeTransformer<QVertex, QEdge>());

		getRenderContext().setEdgeArrowPredicate(
			new Predicate<Context<Graph<QVertex, QEdge>, QEdge>>()
			{
				public boolean evaluate(Context<Graph<QVertex, QEdge>, QEdge> object) {
					return QuantoApp.getInstance().getPreference(QuantoApp.DRAW_ARROW_HEADS);
				}
			});

		getRenderContext().setVertexLabelTransformer(new QVertexLabelTransformer());
		getRenderContext().setVertexLabelRenderer(new QVertexAngleLabeler());
		getRenderContext().setVertexFillPaintTransformer(new QVertexColorTransformer());
		getRenderContext().setVertexShapeTransformer(new QVertexShapeTransformer());

		getRenderer().setVertexRenderer(new QVertexRenderer());
		getRenderer().getVertexLabelRenderer().setPosition(
			VertexLabel.Position.S);

		// For debugging: show a grid behind the graph
		//addPreRenderPaintable(new GridPaintable(new GridPaintable.BoundsCalculator() {
                //              public Rectangle2D getBounds() { return getGraphBounds(); }
                //}));

		addPreRenderPaintable(bangBoxPainter);
	}

	private Dimension calculateGraphSize()
	{
		Dimension size = layout.getSize();
		Rectangle2D rect = new Rectangle2D.Double(0, 0, size.getWidth(), size.getHeight());
		Shape bound = getRenderContext().getMultiLayerTransformer().transform(rect);
		rect = bound.getBounds2D();
		size.setSize(rect.getWidth(), rect.getHeight());
		return size;
	}

	/**
	 * Compute a bounding box and scale such that the largest dimension fits within the
	 * view port.
	 */
	public void zoomToFit(Dimension size) {
		MutableTransformer mt = getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);
		Rectangle2D gb = getGraphBounds();
		double centerX = size.getWidth() / 2.0;
		double centerY = size.getHeight() / 2.0;
		mt.translate(
			centerX - gb.getCenterX(),
			centerY - gb.getCenterY());
		float scale = Math.min(
			(float) (size.getWidth() / gb.getWidth()),
			(float) (size.getHeight() / gb.getHeight()));
		if (scale < 1) {
			mt.scale(scale, scale, new Point2D.Double(centerX, centerY));
		}
		setPreferredSize(size);
	}

	public void unzoom() {
		MutableTransformer mt = getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);
		mt.setToIdentity();
		setPreferredSize(calculateGraphSize());
	}

	public QGraph getGraph() {
		return graph;
	}

	public boolean isLocked(QVertex v) {
		return layout.isLocked(v);
	}

	public void lock(Set<QVertex> verts) {
		for (QVertex v : verts) {
			layout.lock(v, true);
		}
	}

	public void unlock(Set<QVertex> verts) {
		for (QVertex v : verts) {
			layout.lock(v, false);
		}
	}

	public Rectangle2D transformBangBox(QBangBox bb) {
		return layout.transformBangBox(bb);
	}

	public boolean isLayoutSmoothingEnabled() {
		return smoothLayout != null;
	}

	public void setLayoutSmoothingEnabled(boolean enable) {
		if (enable && smoothLayout == null) {
			smoothLayout = new SmoothLayoutDecorator<QVertex,QEdge>(layout);
			smoothLayout.setOrigin(new Point2D.Double(0.0,0.0));
			smoothLayout.initialize();
			super.setGraphLayout(smoothLayout);
		}
		else if (!enable && smoothLayout != null) {
			smoothLayout = null;
			super.setGraphLayout(layout);
		}
	}

	public void setSmoothingOrigin(Point2D o) {
		if (smoothLayout != null)
			smoothLayout.setOrigin(o);
	}

	public void setSmoothingOrigin(double x, double y) {
		if (smoothLayout != null)
			smoothLayout.setOrigin(x, y);
	}

	@Override
	public void setGraphLayout(Layout<QVertex, QEdge> layout) {
		if (!(layout instanceof BangBoxLayout)) {
			throw new IllegalArgumentException("Layout must be a BangBoxLayout");
		}
		super.setGraphLayout(layout);
		this.layout = (BangBoxLayout<QVertex, QEdge, QBangBox>) layout;
	}

	/**
	 * Draw a bounding box around the graph.
	 */
	public void setBoundingBoxEnabled(boolean enabled) {
		if (enabled != boundsPaintingEnabled) {
			if (enabled) {
				if (boundsPaint == null) {
					boundsPaint = new BackdropPaintable(layout);
                                        boundsPaint.setBackgroundColor(new Color(0.99f, 0.99f, 0.99f));
				}
				prependPreRenderPaintable(boundsPaint);
			}
			else {
				removePreRenderPaintable(boundsPaint);
			}
		}
	}

	public boolean isBoundingBoxEnabled() {
		return boundsPaintingEnabled;
	}

	/**
	 * Compute the bounding box of the graph under its current layout.
	 * @return
	 */
	public Rectangle2D getGraphBounds() {
		Rectangle2D bounds = null;
		synchronized (getGraph()) {
			bounds = getSubgraphBounds(getGraphLayout(), getGraph().getVertices());
		}
		return bounds;
	}

        // FIXME: this isn't really the right place
        public static Rectangle2D getSubgraphBounds(
                Layout<QVertex, QEdge> layout,
                Collection<QVertex> subgraph)
        {
		Rectangle2D bounds = null;
                for (QVertex v : subgraph) {
                        Point2D p = layout.transform(v);
                        if (bounds == null) {
                                bounds = new Rectangle2D.Double(p.getX(), p.getY(), 0, 0);
                        }
                        else {
                                bounds.add(p);
                        }
                }
                if (bounds != null) {
                        bounds.setRect(bounds.getX() - 20,
                                       bounds.getY() - 20,
                                       bounds.getWidth() + 40,
                                       bounds.getHeight() + 40);
                }

		if (bounds == null) {
			return new Rectangle2D.Double(0.0d, 0.0d, 20.0d, 20.0d);
		}
		else {
			return bounds;
		}
        }

	/**
	 * Compute the bounding box of the subgraph under its current layout.
	 * @return
	 */
	public Rectangle2D getSubgraphBounds(Collection<QVertex> subgraph) {
		synchronized (getGraph()) {
                        return getSubgraphBounds(getGraphLayout(), subgraph);
                }
	}

	/*@Override
	public Dimension getPreferredSize() {
		return layout.getSize();
	}*/

	public void relayout() {
		getGraphLayout().reset();

		Relaxer relaxer = getModel().getRelaxer();
		if (relaxer != null) {
			relaxer.relax();
		}

		setPreferredSize(calculateGraphSize());

		revalidate();
	}

	public void update() {
		revalidate();
		repaint();
	}

	public void setPickedBangBoxState(PickedState<QBangBox> pickedBangBoxState) {
		if (pickEventListener != null && this.pickedBangBoxState != null) {
			this.pickedBangBoxState.removeItemListener(pickEventListener);
		}
		this.pickedBangBoxState = pickedBangBoxState;
		if (pickEventListener == null) {
			pickEventListener = new ItemListener() {
				public void itemStateChanged(ItemEvent e) {
					repaint();
				}
			};
		}
		pickedBangBoxState.addItemListener(pickEventListener);
		bangBoxPainter.setPickedState(pickedBangBoxState);
	}

	public PickedState<QBangBox> getPickedBangBoxState() {
		return pickedBangBoxState;
	}
}
