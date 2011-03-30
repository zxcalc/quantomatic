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
import quanto.core.data.BasicEdge;
import quanto.core.data.RGVertex;

/**
 *
 * @author alemer
 */
public class QVertexRenderer extends BasicVertexRenderer<RGVertex, BasicEdge>
{
        @Override
        protected void paintShapeForVertex(
                RenderContext<RGVertex,BasicEdge> rc, RGVertex v, Shape shape)
        {
                super.paintShapeForVertex(rc, v, shape);
                if (v.getVertexType() == RGVertex.Type.HADAMARD)
                {
                        GraphicsDecorator g = rc.getGraphicsContext();
                        Paint oldPaint = g.getPaint();
                        g.setPaint(Color.BLACK);
                        Rectangle2D boxRect = shape.getBounds2D();
                        Rectangle2D textRect = g.getFontMetrics().getStringBounds("H", g.getDelegate());
                        double x = boxRect.getCenterX() - textRect.getCenterX();
                        double y = boxRect.getCenterY() - textRect.getCenterY();
                        g.drawString("H", (float)x, (float)y);
                        g.setPaint(oldPaint);
                }
                else if (v.getVertexType() == RGVertex.Type.BOUNDARY)
                {
                        String index = rc.getVertexLabelTransformer().transform(v);
                        GraphicsDecorator g = rc.getGraphicsContext();
                        Paint oldPaint = g.getPaint();
                        g.setPaint(Color.BLACK);
                        Rectangle2D boxRect = shape.getBounds2D();
                        Rectangle2D textRect = g.getFontMetrics().getStringBounds(index, g.getDelegate());
                        double x = boxRect.getCenterX() - textRect.getCenterX();
                        double y = boxRect.getCenterY() - textRect.getCenterY();
                        g.drawString(index, (float)x, (float)y);
                        g.setPaint(oldPaint);
                }
        }
}
