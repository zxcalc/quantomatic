/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxGraphRenderer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.picking.PickedState;

/**
 *
 * @author alemer
 */
public interface BangBoxVisualizationServer<V,E,B> extends VisualizationServer<V, E>
{
	PickedState<B> getPickedBangBoxState();
	void setPickedBangBoxState(PickedState<B> pickedBangBoxState);
	BangBoxGraphElementAccessor<V, E, B> getPickSupport();
	void setPickSupport(BangBoxGraphElementAccessor<V, E, B> pickSupport);
	BangBoxRenderContext<V, E, B> getRenderContext();
	void setRenderContext(BangBoxRenderContext<V, E, B> renderContext);
	BangBoxGraphRenderer<V, E, B> getRenderer();
	void setRenderer(BangBoxGraphRenderer<V, E, B> renderer);
}
