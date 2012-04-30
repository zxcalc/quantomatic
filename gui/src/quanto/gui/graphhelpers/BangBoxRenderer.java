package quanto.gui.graphhelpers;

import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.contrib.visualization.renderers.BasicBangBoxRenderer;

import java.awt.Shape;

import quanto.core.data.Edge;
import quanto.core.data.Vertex;
import quanto.core.data.BangBox;

public class BangBoxRenderer extends BasicBangBoxRenderer<Vertex, Edge, BangBox> 
{
     Shape shape = null;
	@Override
	protected void paintShapeForBangBox(BangBoxGraphRenderContext<Vertex, Edge, BangBox> rc, BangBox b, Shape shape) {
    	
    	super.paintShapeForBangBox((BangBoxGraphRenderContext<Vertex, Edge, BangBox>) rc, b, shape);
    	this.shape = shape;
    }
	
	public Shape getShape() {
	     return this.shape;
	}
}
