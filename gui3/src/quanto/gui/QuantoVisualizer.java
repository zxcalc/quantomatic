package quanto.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.picking.PickedState;

public class QuantoVisualizer extends VisualizationViewer<QVertex,QEdge> {
	private static final long serialVersionUID = -1915610684250038897L;
	public QuantoGraph graph;
	public Layout<QVertex,QEdge> targetLayout;
	
	public QuantoVisualizer(QuantoGraph g) {
		super(new StaticLayout<QVertex,QEdge>(g));
		setGraphLayout(new SmoothLayoutDecorator<QVertex,QEdge>(
				new DotLayout<QVertex,QEdge>(g)));
		Relaxer r = getModel().getRelaxer();
		if (r!= null) r.setSleepTime(4);
		this.graph = g;
		this.targetLayout = getGraphLayout();
		
        setPreferredSize(new Dimension(800,600));
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QVertex,Paint>() {
					public Paint transform(QVertex v) {
						return v.getColor();
					}
        		});
        
        getRenderContext().setEdgeShapeTransformer(
        		new EdgeShape.Line<QVertex,QEdge>());
        
        /*getRenderContext().setVertexLabelTransformer(
        		new ToStringLabeller<QuantoVertex>());*/
        
        getRenderContext().setVertexShapeTransformer(
        		new Transformer<QVertex, Shape>() {
        			public Shape transform(QVertex v) {
        				if (v.getVertexType()==QVertex.Type.BOUNDARY)
        					return new Ellipse2D.Double(-2,-2,4,4);
        				else if (v.getVertexType()==QVertex.Type.HADAMARD)
        					return new Rectangle2D.Double(-7,-7,14,14);
        				else
        					return new Ellipse2D.Double(-7,-7,14,14);
        			}
        		});
        
        final PickedState<QVertex> pickedState = getPickedVertexState(); 
        getRenderContext().setVertexStrokeTransformer(
        		new Transformer<QVertex,Stroke>() {
					public Stroke transform(QVertex v) {
						if (pickedState.isPicked(v)) 
							 return new BasicStroke(3);
						else return new BasicStroke(1);
					}
        		});
        getRenderContext().setVertexDrawPaintTransformer(
        		new Transformer<QVertex, Paint>() {
					public Paint transform(QVertex v) {
						if (pickedState.isPicked(v)) 
							 return Color.blue;
						else return Color.black;
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
