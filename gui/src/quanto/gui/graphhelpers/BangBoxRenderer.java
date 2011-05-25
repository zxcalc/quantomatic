package quanto.gui.graphhelpers;

import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.contrib.visualization.renderers.BasicBangBoxRenderer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;

import java.awt.Color;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

import quanto.core.data.Edge;
import quanto.core.data.Vertex;
import quanto.core.data.BangBox;

public class BangBoxRenderer extends BasicBangBoxRenderer<Vertex, Edge, BangBox> 
{
	@Override
	protected void paintShapeForBangBox(BangBoxGraphRenderContext<Vertex, Edge, BangBox> rc, BangBox b, Shape shape) {
    	
    	super.paintShapeForBangBox((BangBoxGraphRenderContext<Vertex, Edge, BangBox>) rc, b, shape);
    	
        String fillText = b.getCoreName();
        GraphicsDecorator g = rc.getGraphicsContext();
        Paint oldPaint = g.getPaint();
        g.setPaint(Color.BLACK);
        Rectangle2D boxRect = shape.getBounds2D();
        Rectangle2D textRect = g.getFontMetrics().getStringBounds(fillText, g.getDelegate());
        double x = boxRect.getCenterX() - textRect.getCenterX();
        double y = boxRect.getCenterY() - textRect.getCenterY() - 15;
        g.drawString(fillText, (float)x, (float)y);
        g.setPaint(oldPaint);
    }
}
