package quanto.gui.graphhelpers;

import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.renderers.BasicVertexRenderer;
import java.awt.Shape;
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
