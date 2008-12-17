package quanto.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.InputEvent;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.*;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.picking.PickedState;

public class QuantoVisualizer extends VisualizationViewer<QVertex,String> {
	private static final long serialVersionUID = -1915610684250038897L;
	private QuantoConsole console;
	public QuantoGraph graph;
	public Layout<QVertex,String> targetLayout;
	
	private static class RWMouse extends PluggableGraphMouse {
		public RWMouse(QuantoVisualizer vis) {
			int mask = InputEvent.CTRL_MASK;
			if (QuantoFrame.isMac) mask = InputEvent.META_MASK;
			
			add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, 1.1f, 0.909f));
			add(new TranslatingGraphMousePlugin(InputEvent.BUTTON1_MASK | mask));
			add(new PickingGraphMousePlugin<QVertex, String>());
			add(new AddEdgeGraphMousePlugin(vis, InputEvent.BUTTON1_MASK | InputEvent.ALT_MASK));
		}
	}
	
	
	public QuantoVisualizer(QuantoConsole console, QuantoGraph g) {
		super(new StaticLayout<QVertex,String>(g));
		this.console = console;
		setGraphLayout(new SmoothLayoutDecorator<QVertex,String>(
				new DotLayout<QVertex,String>(g)));
		Relaxer r = getModel().getRelaxer();
		if (r!= null) r.setSleepTime(4);
		this.graph = g;
		this.targetLayout = getGraphLayout();
		
		setGraphMouse(new RWMouse(this));
		
        setPreferredSize(new Dimension(800,600));
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QVertex,Paint>() {
					public Paint transform(QVertex v) {
						return v.getColor();
					}
        		});
        
        getRenderContext().setEdgeShapeTransformer(
        		new EdgeShape.Line<QVertex, String>());
        
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
	public QuantoConsole getConsole() {
		return console;
	}
}
