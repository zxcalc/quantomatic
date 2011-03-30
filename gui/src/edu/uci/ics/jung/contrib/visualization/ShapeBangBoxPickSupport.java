/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.picking.ShapePickSupport;
import java.awt.Shape;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.LinkedHashSet;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.functors.ChainedTransformer;
import org.apache.commons.collections15.functors.TruePredicate;

/**
 *
 * @author alemer
 */
public class ShapeBangBoxPickSupport<V, E, B>
	extends ShapePickSupport<V, E>
	implements BangBoxGraphElementAccessor<V, E, B> {

	protected BangBoxVisualizationServer<V, E, B> bvv;

	public ShapeBangBoxPickSupport(BangBoxVisualizationServer<V, E, B> vv) {
		super(vv);
		this.bvv = vv;
	}

	public ShapeBangBoxPickSupport(BangBoxVisualizationServer<V, E, B> vv, float pickSize) {
		super(vv, pickSize);
		this.bvv = vv;
	}

	public B getBangBox(Layout<V, E> layout, double x, double y) {
		if (!(layout.getGraph() instanceof BangBoxGraph)) {
			return null;
		}
		@SuppressWarnings("unchecked")
		BangBoxGraph<V, E, B> graph = (BangBoxGraph<V, E, B>) layout.getGraph();

		B closest = null;
		double minDistance = Double.MAX_VALUE;
		Point2D ip = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(Layer.VIEW,
											       new Point2D.Double(x, y));
		x = ip.getX();
		y = ip.getY();

		while (true) {
			try {
				for (B b : getFilteredBangBoxes(graph)) {

					Shape shape = bvv.getRenderContext().getBangBoxShapeTransformer().transform(
						LayoutContext.<Layout<V,E>,B>getInstance(layout, b));


					if (shape.contains(x, y)) {

						if (style == Style.LOWEST) {
							// return the first match
							return b;
						}
						else if (style == Style.HIGHEST) {
							// will return the last match
							closest = b;
						}
						else {

							// return the !-box closest to the
							// center of a !-box shape
							Rectangle2D bounds = shape.getBounds2D();
							double dx = bounds.getCenterX() - x;
							double dy = bounds.getCenterY() - y;
							double dist = dx * dx + dy * dy;
							if (dist < minDistance) {
								minDistance = dist;
								closest = b;
							}
						}
					}
				}
				break;
			}
			catch (ConcurrentModificationException cme) {
			}
		}
		return closest;

	}

	protected Collection<B> getFilteredBangBoxes(BangBoxGraph<V, E, B> graph) {
		if (bangBoxesAreFiltered()) {
			Collection<B> unfiltered = graph.getBangBoxes();
			Collection<B> filtered = new LinkedHashSet<B>();
			for (B b : unfiltered) {
				if (isBangBoxRendered(Context.<BangBoxGraph<V, E, B>, B>getInstance(graph, b))) {
					filtered.add(b);
				}
			}
			return filtered;
		}
		else {
			return graph.getBangBoxes();
		}
	}

	/**
	 * Quick test to allow optimization of <code>getFilteredVertices()</code>.
	 * @return <code>true</code> if there is an active vertex filtering
	 * mechanism for this visualization, <code>false</code> otherwise
	 */
	protected boolean bangBoxesAreFiltered() {
		Predicate<Context<BangBoxGraph<V, E, B>, B>> bangBoxIncludePredicate =
			bvv.getRenderContext().getBangBoxIncludePredicate();
		return bangBoxIncludePredicate != null
			&& bangBoxIncludePredicate instanceof TruePredicate == false;
	}

	/**
	 * Returns <code>true</code> if this !-box in this graph is included
	 * in the collections of elements to be rendered, and <code>false</code> otherwise.
	 * @param context the vertex and graph to be queried
	 * @return <code>true</code> if this !-box is
	 * included in the collections of elements to be rendered, <code>false</code>
	 * otherwise.
	 */
	protected boolean isBangBoxRendered(Context<BangBoxGraph<V, E, B>, B> context) {
		Predicate<Context<BangBoxGraph<V, E, B>, B>> vertexIncludePredicate =
			bvv.getRenderContext().getBangBoxIncludePredicate();
		return vertexIncludePredicate == null || vertexIncludePredicate.evaluate(context);
	}
}
