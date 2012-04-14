/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.visualization.Layer;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alemer
 */
public class DefaultBangBoxShapeTransformer<V, E, B>
	implements Transformer<LayoutContext<Layout<V, E>, B>, Shape> {

	protected double padding;
	protected BangBoxGraphRenderContext<V, E, B> renderContext;

	public DefaultBangBoxShapeTransformer(BangBoxGraphRenderContext<V, E, B> renderContext, double padding) {
		this.renderContext = renderContext;
		this.padding = padding;
	}

	public Shape transform(LayoutContext<Layout<V, E>, B> context) {
		@SuppressWarnings("unchecked")
		BangBoxGraph<V, E, B> graph = (BangBoxGraph<V, E, B>) context.layout.getGraph();
		Rectangle2D rect = vertexBounds(context.layout, graph.getBoxedVertices(context.element));
        if (rect != null) {
            rect.setRect(rect.getX() - padding,
                     rect.getY() - padding,
                     rect.getWidth() + 2*padding,
                     rect.getHeight() + 2*padding);
        } else {
            rect = new Rectangle2D.Double(0, 0, 0, 0);
        }
		return rect;
	}

	protected Rectangle2D vertexBounds(Layout<V, E> layout, Collection<V> vertices) {
		Rectangle2D rect = null;
		for (V v : vertices) {
			Point2D p = layout.transform(v);
			p = renderContext.getMultiLayerTransformer().transform(Layer.LAYOUT, p);
			AffineTransform xform = AffineTransform.getTranslateInstance(p.getX(), p.getY());
			Shape shape = xform.createTransformedShape(renderContext.getVertexShapeTransformer().transform(v));
			if (rect == null)
				rect = shape.getBounds2D();
			else
				rect.add(shape.getBounds2D());
		}
		return rect;
	}

}
