package quanto.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.contrib.DotLayout;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.control.ScalingControl;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;

public class GraphView extends VisualizationViewer<QVertex,QEdge> {
	private static final long serialVersionUID = -1915610684250038897L;
	public QuantoGraph graph;
	private VisualizationServer.Paintable boundsPaint;
	
	public GraphView(QuantoGraph g) {
		this(g, new Dimension(800, 600));
	}
	
	public GraphView(QuantoGraph g, Dimension size) {
		super(new DotLayout<QVertex,QEdge>(g, size));
		setPreferredSize(size);
		getGraphLayout().initialize();
		this.graph = g;
		
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QVertex,Paint>() {
					public Paint transform(QVertex v) {
						return v.getColor();
					}
        		});
        
        
        getRenderContext().setParallelEdgeIndexFunction(
        		BalancedEdgeIndexFunction.<QVertex,QEdge>getInstance());
        
        getRenderContext().setEdgeShapeTransformer(
        		new EdgeShape.QuadCurve<QVertex,QEdge>());
        
        getRenderContext().setVertexLabelTransformer(
        		new Transformer<QVertex, String>() {
					public String transform(QVertex v) {
						if (v.getVertexType()==QVertex.Type.BOUNDARY)
							return String.format("(%d)",
									getGraph().getBoundaryIndex(v));
						else return v.getAngle();
					}
        		});
        
        getRenderContext().setVertexShapeTransformer(
        		new Transformer<QVertex, Shape>() {
        			public Shape transform(QVertex v) {
        				if (v.getVertexType()==QVertex.Type.BOUNDARY)
        					return new Ellipse2D.Double(-4,-4,8,8);
        				else if (v.getVertexType()==QVertex.Type.HADAMARD)
        					return new Rectangle2D.Double(-7,-7,14,14);
        				else
        					return new Ellipse2D.Double(-7,-7,14,14);
        			}
        		});
	}
	
	/**
	 * Draw a bounding box around the graph.
	 */
	public void enableBoundingBox() {
		if (boundsPaint == null) boundsPaint = new BoundsPaintable();
		addPostRenderPaintable(boundsPaint);
	}
	
	/**
	 * Disable the bounding box.
	 */
	public void disableBoundingBox() {
		removePostRenderPaintable(boundsPaint);
	}
	
	public QuantoGraph getGraph() {
		return graph;
	}
	public void setGraph(QuantoGraph graph) {
		this.graph = graph;
	}
	
	/**
	 * Compute the bounding box of the graph under its current layout.
	 * @return
	 */
	public Rectangle2D getGraphBounds() {
		double top=0, left=0, right=0, bottom=0;
		Layout<QVertex, QEdge> layout = getGraphLayout();
		synchronized (getGraph()) {
			if (getGraph().getVertexCount()==0) {
				left = 0d; top = 0d; right = 20d; bottom = 20d;
			} else {
				boolean init = true;
				for (QVertex v : getGraph().getVertices()) {
					Point2D p = layout.transform(v);
					if (init || p.getY()<top) top = p.getY();
					if (init || p.getY()>bottom) bottom = p.getY();
					if (init || p.getX()<left) left = p.getX();
					if (init || p.getX()>right) right = p.getX();
					init = false;
				}
			}
		}
		return new Rectangle2D.Double(
				left-20d, top-20d, right-left+40d, bottom-top+40d);
	}
	
	/**
	 * Compute a bounding box and scale such that the largest dimension fits within the
	 * view port.
	 */
	public void zoomToFit() {
		ScalingControl sc = new CrossoverScalingControl();
		Dimension vb = getPreferredSize();
		Rectangle2D gb = getGraphBounds();
		float scale = Math.min(
				(float)(vb.getWidth() / gb.getWidth()),
				(float)(vb.getHeight() / gb.getHeight()));
		sc.scale(this, scale, new Point2D.Double(gb.getCenterX(), gb.getCenterY()));
	}
	
	/**
	 * A red box that surrounds the graph. NOTE: this doesn't appear correctly if the graph is
	 * transformed.
	 */
	class BoundsPaintable implements VisualizationServer.Paintable {
        public void paint(Graphics g) {
        	Color oldColor = g.getColor();
            g.setColor(Color.red);
        	((Graphics2D)g).draw(getGraphBounds());
        	g.setColor(oldColor);
        }
        
        public boolean useTransform() {return false;}
    }
}
