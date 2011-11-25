/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization.control;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphElementAccessor;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphVisualizationViewer;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin;
import edu.uci.ics.jung.visualization.picking.PickedState;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;

/**
 * !-box aware version of PickingGraphMousePlugin
 *
 * @author alemer
 */
public class PickingBangBoxMousePlugin<V, E, B>
	extends PickingGraphMousePlugin<V, E> {

	B bangBox = null;

	@SuppressWarnings("unchecked")
	@Override
	public void mousePressed(MouseEvent e) {
		down = e.getPoint();
		BangBoxGraphVisualizationViewer<V, E, B> vv = (BangBoxGraphVisualizationViewer) e.getSource();
		BangBoxGraphElementAccessor<V, E, B> pickSupport = vv.getPickSupport();
		PickedState<V> pickedVertexState = vv.getPickedVertexState();
		PickedState<E> pickedEdgeState = vv.getPickedEdgeState();
		PickedState<B> pickedBangBoxState = vv.getPickedBangBoxState();
		if (pickSupport != null && pickedVertexState != null) {
			Layout<V, E> layout = vv.getGraphLayout();
			if (e.getModifiers() == modifiers) {
				rect.setFrameFromDiagonal(down, down);
				// p is the screen point for the mouse event
				Point2D ip = e.getPoint();

				vertex = pickSupport.getVertex(layout, ip.getX(), ip.getY());
				if (vertex != null) {
					if (pickedVertexState.isPicked(vertex) == false) {
						pickedVertexState.clear();
						pickedEdgeState.clear();
						pickedBangBoxState.clear();
						pickedVertexState.pick(vertex, true);
					}
					// layout.getLocation applies the layout transformer so
					// q is transformed by the layout transformer only
					Point2D q = layout.transform(vertex);
					// transform the mouse point to graph coordinate system
					Point2D gp = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(Layer.LAYOUT, ip);

					offsetx = (float) (gp.getX() - q.getX());
					offsety = (float) (gp.getY() - q.getY());
				}
				else if ((edge = pickSupport.getEdge(layout, ip.getX(), ip.getY())) != null) {
					pickedVertexState.clear();
					pickedEdgeState.clear();
					pickedBangBoxState.clear();
					pickedEdgeState.pick(edge, true);
				}
				else if ((bangBox = pickSupport.getBangBox(layout, ip.getX(), ip.getY())) != null) {
					pickedVertexState.clear();
					pickedEdgeState.clear();
					pickedBangBoxState.clear();
					pickedBangBoxState.pick(bangBox, true);
				}
				else {
					vv.addPostRenderPaintable(lensPaintable);
					pickedVertexState.clear();
					pickedEdgeState.clear();
					pickedBangBoxState.clear();
				}

			}
			else if (e.getModifiers() == addToSelectionModifiers) {
				vv.addPostRenderPaintable(lensPaintable);
				rect.setFrameFromDiagonal(down, down);
				Point2D ip = e.getPoint();
				vertex = pickSupport.getVertex(layout, ip.getX(), ip.getY());
				if (vertex != null) {
					boolean wasThere = pickedVertexState.pick(vertex, !pickedVertexState.isPicked(vertex));
					if (wasThere) {
						vertex = null;
					}
					else {

						// layout.getLocation applies the layout transformer so
						// q is transformed by the layout transformer only
						Point2D q = layout.transform(vertex);
						// translate mouse point to graph coord system
						Point2D gp = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(Layer.LAYOUT, ip);

						offsetx = (float) (gp.getX() - q.getX());
						offsety = (float) (gp.getY() - q.getY());
					}
				}
				else if ((edge = pickSupport.getEdge(layout, ip.getX(), ip.getY())) != null) {
					pickedEdgeState.pick(edge, !pickedEdgeState.isPicked(edge));
				}
				else if ((bangBox = pickSupport.getBangBox(layout, ip.getX(), ip.getY())) != null) {
					pickedBangBoxState.pick(bangBox, !pickedBangBoxState.isPicked(bangBox));
				}
			}
		}
		if (vertex != null) {
			e.consume();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		bangBox = null;
	}
}
