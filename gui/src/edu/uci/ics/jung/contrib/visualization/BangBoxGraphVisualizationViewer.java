/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.visualization.decorators.PickableBangBoxPaintTransformer;
import edu.uci.ics.jung.contrib.visualization.decorators.PickableElementStrokeTransformer;
import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxGraphRenderer;
import edu.uci.ics.jung.contrib.visualization.renderers.BasicBangBoxGraphRenderer;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.VisualizationModel;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.PickableEdgePaintTransformer;
import edu.uci.ics.jung.visualization.decorators.PickableVertexPaintTransformer;
import edu.uci.ics.jung.visualization.picking.MultiPickedState;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.Renderer;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 *
 * @author alemer
 */
public class BangBoxGraphVisualizationViewer<V, E, B>
	extends VisualizationViewer<V, E>
	implements BangBoxGraphVisualizationServer<V, E, B> {

	protected PickedState<B> pickedBangBoxState = new MultiPickedState<B>();

	public BangBoxGraphVisualizationViewer(Layout<V, E> layout) {
		super(layout);
		init();
	}

	public BangBoxGraphVisualizationViewer(Layout<V, E> layout, Dimension preferredSize) {
		super(layout, preferredSize);
		init();
	}

	public BangBoxGraphVisualizationViewer(VisualizationModel<V, E> model) {
		super(model);
		init();
	}

	public BangBoxGraphVisualizationViewer(VisualizationModel<V, E> model, Dimension preferredSize) {
		super(model, preferredSize);
		init();
	}

	private void init() {
		PluggableBangBoxGraphRenderContext<V, E, B> prc = new PluggableBangBoxGraphRenderContext<V, E, B>();
		renderContext = prc;
		renderer = new BasicBangBoxGraphRenderer<V, E, B>();
		setPickSupport(new ShapeBangBoxPickSupport<V, E, B>(this));
		setPickedVertexState(new MultiPickedState<V>());
		setPickedEdgeState(new MultiPickedState<E>());
		setPickedBangBoxState(new MultiPickedState<B>());
		renderContext.setEdgeDrawPaintTransformer(new PickableEdgePaintTransformer<E>(getPickedEdgeState(), Color.black, Color.cyan));
		renderContext.setVertexFillPaintTransformer(new PickableVertexPaintTransformer<V>(getPickedVertexState(),
												  Color.red, Color.yellow));
		prc.setBangBoxDrawPaintTransformer(new PickableBangBoxPaintTransformer<B>(getPickedBangBoxState(), Color.gray, Color.blue));
		prc.setBangBoxStrokeTransformer(new PickableElementStrokeTransformer<B>(getPickedBangBoxState(), new BasicStroke(1), new BasicStroke(2)));
		renderContext.getMultiLayerTransformer().addChangeListener(this);
	}

	public PickedState<B> getPickedBangBoxState() {
		return pickedBangBoxState;
	}

	public void setPickedBangBoxState(PickedState<B> pickedBangBoxState) {
		if (pickEventListener != null && this.pickedBangBoxState != null) {
			this.pickedBangBoxState.removeItemListener(pickEventListener);
		}
		this.pickedBangBoxState = pickedBangBoxState;
		getRenderContext().setPickedBangBoxState(pickedBangBoxState);
		if (pickEventListener == null) {
			pickEventListener = new ItemListener() {

				public void itemStateChanged(ItemEvent e) {
					repaint();
				}
			};
		}
		pickedBangBoxState.addItemListener(pickEventListener);
	}

	@Override
	public BangBoxGraphElementAccessor<V, E, B> getPickSupport() {
		return getRenderContext().getPickSupport();
	}

	public void setPickSupport(BangBoxGraphElementAccessor<V, E, B> pickSupport) {
		super.setPickSupport(pickSupport);
	}

	@Override
	public void setPickSupport(GraphElementAccessor<V, E> pickSupport) {
		if (pickSupport instanceof BangBoxGraphElementAccessor) {
			super.setPickSupport(pickSupport);
		}
		else if (super.getPickSupport() != null) {
			// ^^ this is for when the BVS constructor calls this
			throw new IllegalArgumentException("renderer must be a BangBoxGraphElementAccessor");
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public BangBoxGraphRenderContext<V, E, B> getRenderContext() {
		return (BangBoxGraphRenderContext<V, E, B>) renderContext;
	}

	public void setRenderContext(BangBoxGraphRenderContext<V, E, B> renderContext) {
		super.setRenderContext(renderContext);
	}

	@Override
	public void setRenderContext(RenderContext<V, E> renderContext) {
		if (!(renderContext instanceof RenderContext)) {
			throw new IllegalArgumentException("renderer must be a RenderContext");
		}
		super.setRenderContext(renderContext);
	}

	@Override
	@SuppressWarnings("unchecked")
	public BangBoxGraphRenderer<V, E, B> getRenderer() {
		return (BangBoxGraphRenderer<V, E, B>) renderer;
	}

	public void setRenderer(BangBoxGraphRenderer<V, E, B> renderer) {
		super.setRenderer(renderer);
	}

	@Override
	public void setRenderer(Renderer<V, E> renderer) {
		if (!(renderer instanceof BangBoxGraphRenderer)) {
			throw new IllegalArgumentException("renderer must be a BangBoxGraphRenderer");
		}
		super.setRenderer(renderer);
	}
}
