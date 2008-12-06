package quanto.gui;

import java.awt.Paint;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;

public class QuantoVisualizer extends VisualizationViewer<QuantoVertex,String> {
	private static final long serialVersionUID = -1915610684250038897L;
	public QuantoGraph graph;
	public Layout<QuantoVertex,String> targetLayout;
	public QuantoVisualizer(QuantoGraph g) {
		super(new SmoothLayoutDecorator<QuantoVertex,String>(
				new DotLayout<QuantoVertex,String>(g)));
		Relaxer r = getModel().getRelaxer();
		if (r!= null) r.setSleepTime(4);
		this.graph = g;
		this.targetLayout = getGraphLayout();
		setGraphMouse(new DefaultModalGraphMouse<QuantoVertex, String>());
        
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QuantoVertex,Paint>() {
					public Paint transform(QuantoVertex v) {
						return v.getColor();
					}
        		});
        
        getRenderContext().setEdgeShapeTransformer(
        		new EdgeShape.Line<QuantoVertex, String>());
        
	}
	public QuantoGraph getGraph() {
		return graph;
	}
	public void setGraph(QuantoGraph graph) {
		this.graph = graph;
	}
	
	/*public void animateTo(Layout<QuantoVertex,String> layout) {
		Relaxer relaxer = new VisRunner((IterativeContext)layout);
		relaxer.stop();
		relaxer.prerelax();
		StaticLayout<QuantoVertex,String> staticLayout =
			new StaticLayout<QuantoVertex,String>(graph, layout);
		LayoutTransition<QuantoVertex,String> lt =
			new LayoutTransition<QuantoVertex,String>(this, this.getGraphLayout(),
					staticLayout);
		Animator animator = new Animator(lt);
		animator.start();
		repaint();
	}*/

	public void animateToNewLayout() {
		targetLayout.initialize();
        
		/*Relaxer relaxer = new VisRunner((IterativeContext)targetLayout);
		relaxer.stop();
		relaxer.prerelax();
		StaticLayout<QuantoVertex,String> staticLayout =
			new StaticLayout<QuantoVertex,String>(graph, targetLayout);
		LayoutTransition<QuantoVertex,String> lt =
			new LayoutTransition<QuantoVertex,String>(this, getGraphLayout(),
					staticLayout);
		Animator animator = new Animator(lt);
		animator.start();*/
		repaint();
	}
}
