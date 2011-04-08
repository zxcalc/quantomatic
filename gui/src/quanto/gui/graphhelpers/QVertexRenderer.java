/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.renderers.BasicVertexRenderer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import java.awt.Color;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;

/**
 *
 * @author alemer
 */
public class QVertexRenderer extends BasicVertexRenderer<Vertex, Edge>
{
        @Override
        protected void paintShapeForVertex(
                RenderContext<Vertex,Edge> rc, Vertex v, Shape shape)
        {
                super.paintShapeForVertex(rc, v, shape);
		String fillText = null;
                if (v.isBoundaryVertex()) {
			fillText = v.getCoreName();
		} else {
			fillText = v.getVertexType().getVisualizationData().fillText();
		}
		if (fillText != null) {
                        GraphicsDecorator g = rc.getGraphicsContext();
                        Paint oldPaint = g.getPaint();
                        g.setPaint(Color.BLACK);
                        Rectangle2D boxRect = shape.getBounds2D();
                        Rectangle2D textRect = g.getFontMetrics().getStringBounds(fillText, g.getDelegate());
                        double x = boxRect.getCenterX() - textRect.getCenterX();
                        double y = boxRect.getCenterY() - textRect.getCenterY();
                        g.drawString(fillText, (float)x, (float)y);
                        g.setPaint(oldPaint);
                }
        }
}
