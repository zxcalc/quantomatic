/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization.renderers;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.visualization.renderers.Renderer;

/**
 *
 * @author alemer
 */
public interface BangBoxGraphRenderer<V, E, B> extends Renderer<V, E>
{
	interface BangBox<V, E, B> {
		class NOOP implements BangBox {
			public void paintBangBox(BangBoxGraphRenderContext rc, Layout layout, Object b) {
			}
		}
		void paintBangBox(BangBoxGraphRenderContext<V, E, B> rc, Layout<V, E> layout, B b);
	}
	interface BangBoxLabel<V, E, B> {
		class NOOP implements BangBoxLabel {
               public void labelBangBox(BangBoxGraphRenderContext rc,
                         Layout layout, Object e, String label) {
               }
		}
		void labelBangBox(BangBoxGraphRenderContext<V, E, B> rc, Layout<V,E> layout, B e, String label);
	}

	void render(BangBoxGraphRenderContext<V, E, B> renderContext, Layout<V, E> layout);

	BangBox<V, E, B> getBangBoxRenderer();
	void setBangBoxRenderer(BangBox<V, E, B> bangBoxRenderer);

	BangBoxLabel<V, E, B> getBangBoxLabelRenderer();
	void setBangBoxLabelRenderer(BangBoxLabel<V, E, B> bangBoxLabelRenderer);
	void renderBangBox(BangBoxGraphRenderContext<V, E, B> rc, Layout<V, E> layout, B b);

	void renderBangBoxLabel(BangBoxGraphRenderContext<V, E, B> rc, Layout<V, E> layout, B b);
}
