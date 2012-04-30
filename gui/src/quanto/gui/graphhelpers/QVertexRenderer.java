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
        }
}
