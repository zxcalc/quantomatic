/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import javax.swing.event.ChangeEvent;

import quanto.core.data.BangBox;
import quanto.core.data.Vertex;
import quanto.core.data.Edge;
import quanto.core.data.CoreGraph;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.graph.util.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.contrib.visualization.decorators.MixedShapeTransformer;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphVisualizationViewer;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Shape;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;

import javax.swing.event.ChangeListener;
import org.apache.commons.collections15.Predicate;
import quanto.gui.graphhelpers.QVertexAngleLabeler;
import quanto.gui.graphhelpers.BackdropPaintable;
import quanto.gui.graphhelpers.QVertexColorTransformer;
import quanto.gui.graphhelpers.QVertexIconTransformer;
import quanto.gui.graphhelpers.QVertexLabelTransformer;
import quanto.gui.graphhelpers.QVertexRenderer;
import quanto.gui.graphhelpers.QVertexShapeTransformer;
import quanto.gui.graphhelpers.BangBoxRenderer;

/**
 *
 * @author alex
 */
public class GraphVisualizationViewer
       extends BangBoxGraphVisualizationViewer<Vertex, Edge, BangBox>
{
	private static final long serialVersionUID = -1723894723956293847L;
	private CoreGraph graph;
	private BackdropPaintable boundsPaint;
	private boolean boundsPaintingEnabled = false;
//private QuantoForceLayout layout;
	
	public GraphVisualizationViewer(CoreGraph graph) {
	//	this(new QuantoForceLayout(graph, new QuantoDotLayout(graph)));
		this(QuantoApp.useExperimentalLayout ? new JavaQuantoDotLayout(graph) : new QuantoDotLayout(graph));
	}

	public GraphVisualizationViewer(Layout<Vertex, Edge> layout) {
		super(layout);
		if (!(layout.getGraph() instanceof CoreGraph)) {
			throw new IllegalArgumentException("Only QuantoGraphs are supported");
		}
		this.graph = (CoreGraph) layout.getGraph();
		layout.initialize();
        setBackground(Color.white);

		setupRendering();

		setPreferredSize(calculateGraphSize());

		graph.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				modifyLayout();
                fireStateChanged();
			}
		});
	}

	private void  setupRendering() {
		getRenderContext().setParallelEdgeIndexFunction(
			BalancedEdgeIndexFunction.<Vertex, Edge>getInstance());

		getRenderContext().setEdgeShapeTransformer(
			new MixedShapeTransformer<Vertex, Edge>());

		getRenderContext().setEdgeArrowPredicate(
			new Predicate<Context<Graph<Vertex, Edge>, Edge>>()
			{
				public boolean evaluate(Context<Graph<Vertex, Edge>, Edge> object) {
					return object.element.isDirected();
				}
			});

		getRenderContext().setVertexLabelTransformer(new QVertexLabelTransformer());
		getRenderContext().setVertexLabelRenderer(new QVertexAngleLabeler());
		getRenderContext().setVertexFillPaintTransformer(new QVertexColorTransformer());
		getRenderContext().setVertexShapeTransformer(new QVertexShapeTransformer());
		getRenderContext().setVertexIconTransformer(new QVertexIconTransformer());

		getRenderer().setVertexRenderer(new QVertexRenderer());
		getRenderer().getVertexLabelRenderer().setPosition(
			VertexLabel.Position.S);
      getRenderer().setBangBoxRenderer(new BangBoxRenderer());
		// For debugging: show a grid behind the graph
		//addPreRenderPaintable(new GridPaintable(new GridPaintable.BoundsCalculator() {
                //              public Rectangle2D getBounds() { return getGraphBounds(); }
                //}));
	}

	private Dimension calculateGraphSize()
	{
		Dimension size = getGraphLayout().getSize();
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

	public CoreGraph getGraph() {
		return graph;
	}
	
	public void setCoreGraph(CoreGraph g) {
		this.graph = g;
	}
	
	/**
	 * Draw a bounding box around the graph.
	 */
	public void setBoundingBoxEnabled(boolean enabled) {
		if (enabled != boundsPaintingEnabled) {
			if (enabled) {
				if (boundsPaint == null) {
					boundsPaint = new BackdropPaintable(getGraphLayout());
                                        boundsPaint.setBackgroundColor(new Color(0.99f, 0.99f, 0.99f));
				}
                setBackground(new Color(0.97f, 0.97f, 0.97f));
				prependPreRenderPaintable(boundsPaint);
			}
			else {
                setBackground(Color.white);
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
                Layout<Vertex, Edge> layout,
                Collection<Vertex> subgraph)
        {
		Rectangle2D bounds = null;
                for (Vertex v : subgraph) {
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
	public Rectangle2D getSubgraphBounds(Collection<Vertex> subgraph) {
		synchronized (getGraph()) {
                        return getSubgraphBounds(getGraphLayout(), subgraph);
                }
	}

	/*@Override
	public Dimension getPreferredSize() {
		return layout.getSize();
	}*/

	public void shift(Rectangle2D rect, Vertex v, Point2D shift) {
		
		getGraphLayout().setLocation(v, new Point2D.Double(
				 rect.getCenterX()+shift.getX(), rect.getCenterY()+shift.getY()));
	}
	

	public void modifyLayout() {
		getGraphLayout().reset();	
		update();
	}

	public void update() {
		Relaxer relaxer = getModel().getRelaxer();
		if (relaxer != null) {
			relaxer.relax();
		}
		setPreferredSize(calculateGraphSize());
		Collection<Vertex> c = getGraph().getVertices();
		for(Vertex v : getGraph().getVertices()) {
			v.setPosition(getGraphLayout().transform(v));
		}
		revalidate();
		repaint();
	}

	
}
