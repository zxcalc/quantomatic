/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.picking.PickedState;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alemer
 */
public interface BangBoxRenderContext<V, E, B> extends RenderContext<V, E> {

	PickedState<B> getPickedBangBoxState();
	void setPickedBangBoxState(PickedState<B> pickedBangBoxState);

	BangBoxGraphElementAccessor<V, E, B> getPickSupport();
	void setPickSupport(BangBoxGraphElementAccessor<V, E, B> pickSupport);

	Transformer<B, Stroke> getBangBoxStrokeTransformer();
	void setBangBoxStrokeTransformer(Transformer<B, Stroke> bangBoxStrokeTransformer);

	//Transformer<B,String> getBangBoxLabelTransformer();
	//void setBangBoxLabelTransformer(Transformer<B,String> bangBoxLabelTransformer);

	//BangBoxLabelRenderer getBangBoxLabelRenderer();
	//void getBangBoxLabelRenderer(BangBoxLabelRenderer bangBoxLabelRenderer);

	//Transformer<B,Font> getBangBoxFontTransformer();
	//void setBangBoxFontTransformer(Transformer<B,Font> bangBoxFontTransformer);

	Predicate<Context<BangBoxGraph<V, E, B>, B>> getBangBoxIncludePredicate();
	void setBangBoxIncludePredicate(Predicate<Context<BangBoxGraph<V, E, B>, B>> bangBoxIncludePredicate);

	// NB: unlike the vertex and edge shape transforms, this one will not be
	//     translated or scaled
	Transformer<LayoutContext<Layout<V, E>, B>, Shape> getBangBoxShapeTransformer();
	void setBangBoxShapeTransformer(Transformer<LayoutContext<Layout<V, E>, B>, Shape> bangBoxShapeTransformer);

	Transformer<B, Paint> getBangBoxFillPaintTransformer();
	void setBangBoxFillPaintTransformer(Transformer<B, Paint> bangBoxFillPaintTransformer);

	Transformer<B, Paint> getBangBoxDrawPaintTransformer();
	void setBangBoxDrawPaintTransformer(Transformer<B, Paint> bangBoxDrawPaintTransformer);
}
