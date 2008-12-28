package quanto.gui;

import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.contrib.DotLayout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;

public class QuantoVisualizer extends VisualizationViewer<QVertex,QEdge> {
	private static final long serialVersionUID = -1915610684250038897L;
	public QuantoGraph graph;
	public Layout<QVertex,QEdge> targetLayout;
	
	public QuantoVisualizer(QuantoGraph g) {
		this(g, new Dimension(800, 600));
	}
	
	public QuantoVisualizer(QuantoGraph g, Dimension size) {
		super(new DotLayout<QVertex,QEdge>(g, size));
		setPreferredSize(size);
		getGraphLayout().initialize();
		this.graph = g;
		this.targetLayout = getGraphLayout();
		
		
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QVertex,Paint>() {
					public Paint transform(QVertex v) {
						return v.getColor();
					}
        		});
        
        getRenderContext().setEdgeShapeTransformer(
        		new EdgeShape.Line<QVertex,QEdge>());
        
        getRenderContext().setVertexLabelTransformer(
        		new Transformer<QVertex, String>() {
					public String transform(QVertex v) {
						return v.getAngle();
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
	public QuantoGraph getGraph() {
		return graph;
	}
	public void setGraph(QuantoGraph graph) {
		this.graph = graph;
	}
}
