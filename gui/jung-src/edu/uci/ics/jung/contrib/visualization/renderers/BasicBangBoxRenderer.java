/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization.renderers;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.contrib.visualization.LayoutContext;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxGraphRenderer.BangBox;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import edu.uci.ics.jung.visualization.transform.MutableTransformerDecorator;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import javax.swing.JComponent;

/**
 *
 * @author alemer
 */
public class BasicBangBoxRenderer<V, E, B>
	implements BangBoxGraphRenderer.BangBox<V, E, B> {

	public void paintBangBox(BangBoxGraphRenderContext<V, E, B> rc, Layout<V, E> layout, B b) {
		@SuppressWarnings("unchecked")
		BangBoxGraph<V, E, B> graph = (BangBoxGraph<V, E, B>) layout.getGraph();
		if (!rc.getBangBoxIncludePredicate().evaluate(Context.<BangBoxGraph<V, E, B>, B>getInstance(graph, b))) {
			return;
		}
		Shape shape = rc.getBangBoxShapeTransformer().transform(LayoutContext.<Layout<V, E>,B>getInstance(layout, b));
		if (isOnScreen(rc, shape)) {
			paintShapeForBangBox(rc, b, shape);
		}
	}

	protected boolean isOnScreen(RenderContext<V, E> rc, Shape s) {
		JComponent vv = rc.getScreenDevice();
		Rectangle deviceRectangle = null;
		if (vv != null) {
			Dimension d = vv.getSize();
			deviceRectangle = new Rectangle(
				0, 0,
				d.width, d.height);
		}
		MutableTransformer vt = rc.getMultiLayerTransformer().getTransformer(Layer.VIEW);
		if (vt instanceof MutableTransformerDecorator) {
			vt = ((MutableTransformerDecorator) vt).getDelegate();
		}
		return vt.transform(s).intersects(deviceRectangle);
	}

	protected void paintShapeForBangBox(BangBoxGraphRenderContext<V, E, B> rc, B b, Shape shape) {
		GraphicsDecorator g = rc.getGraphicsContext();
		Paint oldPaint = g.getPaint();
		Paint fillPaint = rc.getBangBoxFillPaintTransformer().transform(b);
		if (fillPaint != null) {
			g.setPaint(fillPaint);
			g.fill(shape);
			g.setPaint(oldPaint);
		}
		Paint drawPaint = rc.getBangBoxDrawPaintTransformer().transform(b);
		if (drawPaint != null) {
			g.setPaint(drawPaint);
			Stroke oldStroke = g.getStroke();
			Stroke stroke = rc.getBangBoxStrokeTransformer().transform(b);
			if (stroke != null) {
				g.setStroke(stroke);
			}
			g.draw(shape);
			g.setPaint(oldPaint);
			g.setStroke(oldStroke);
		}
	}
}
