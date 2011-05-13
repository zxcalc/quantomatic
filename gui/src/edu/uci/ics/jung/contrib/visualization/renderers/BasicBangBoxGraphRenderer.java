/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization.renderers;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxGraphRenderer.BangBox;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.renderers.BasicRenderer;
import java.util.ConcurrentModificationException;

/**
 *
 * @author alemer
 */
public class BasicBangBoxGraphRenderer<V, E, B>
	extends BasicRenderer<V, E>
	implements BangBoxGraphRenderer<V, E, B> {

	protected BangBox<V, E, B> bangBoxRenderer = new BasicBangBoxRenderer<V, E, B>();

	@Override
	@SuppressWarnings("unchecked")
	public void render(RenderContext<V, E> renderContext, Layout<V, E> layout) {
		if (renderContext instanceof BangBoxGraphRenderContext) {
			render((BangBoxGraphRenderContext<V, E, B>)renderContext, layout);
		} else {
			super.render(renderContext, layout);
		}
	}

	public void render(BangBoxGraphRenderContext<V, E, B> renderContext, Layout<V, E> layout) {
		if (layout.getGraph() instanceof BangBoxGraph) {
			// paint all the !-boxes
			@SuppressWarnings("unchecked")
			BangBoxGraph<V, E, B> graph = (BangBoxGraph<V, E, B>)layout.getGraph();
			try {
				for (B b : graph.getBangBoxes()) {

					renderBangBox(
						renderContext,
						layout,
						b);
//					renderBangBoxLabel(
//						rc,
//						layout,
//						b);
				}
			}
			catch (ConcurrentModificationException cme) {
				renderContext.getScreenDevice().repaint();
			}
		}

		// paint all the edges
		try {
			for (E e : layout.getGraph().getEdges()) {

				renderEdge(
					renderContext,
					layout,
					e);
				renderEdgeLabel(
					renderContext,
					layout,
					e);
			}
		}
		catch (ConcurrentModificationException cme) {
			renderContext.getScreenDevice().repaint();
		}

		// paint all the vertices
		try {
			for (V v : layout.getGraph().getVertices()) {

				renderVertex(
					renderContext,
					layout,
					v);
				renderVertexLabel(
					renderContext,
					layout,
					v);
			}
		}
		catch (ConcurrentModificationException cme) {
			renderContext.getScreenDevice().repaint();
		}
	}

	public void renderBangBox(BangBoxGraphRenderContext<V, E, B> rc, Layout<V, E> layout, B b) {
		bangBoxRenderer.paintBangBox(rc, layout, b);
	}

	//public void renderBangBoxLabel(BangBoxRenderContext<V, E> rc, Layout<V, E> layout, B b) {
	//	bangBoxRenderer.paintBangBox(rc, layout, b, rc.getBangBoxLabelTransformer().transform(b));
	//}

	public BangBox<V, E, B> getBangBoxRenderer() {
		return bangBoxRenderer;
	}

	public void setBangBoxRenderer(BangBox<V, E, B> bangBoxRenderer) {
		this.bangBoxRenderer = bangBoxRenderer;
	}
}
