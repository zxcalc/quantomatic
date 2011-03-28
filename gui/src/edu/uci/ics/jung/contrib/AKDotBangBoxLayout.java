/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author alex
 */
public class AKDotBangBoxLayout<V,E,B>
	extends AKDotLayout<V,E>
	implements BangBoxLayout<V, E, B> {

	protected BangBoxLayoutManager<V,E,B> bbLayout;

	public AKDotBangBoxLayout(DirectedBangBoxGraph<V, E, B> graph, double bangBoxPadding) {
		super(graph);
		bbLayout = new BangBoxLayoutManager<V, E, B>(this, bangBoxPadding);
	}

	@Override
	@SuppressWarnings("unchecked")
	public DirectedBangBoxGraph<V, E, B> getGraph() {
		return (DirectedBangBoxGraph<V, E, B>)super.getGraph();
	}

	@Override
	public void initialize() {
		super.initialize();
		updateBangBoxes();
	}

	protected void updateBangBoxes() {
		synchronized (getGraph()) {
			for (B bb : getGraph().getBangBoxes()) {
				Collection<V> contents = (getGraph()).getBoxedVertices(bb);
				if (! contents.isEmpty()) {
					bbLayout.recalculateNonEmptyBangBox(bb);
				}
			}
		}
	}

	public Rectangle2D transformBangBox(B bb) {
		return bbLayout.transform(bb);
	}

	@Override
	public void setLocation(V picked, Point2D p) {
		super.setLocation(picked, p);
		Dimension newSize = bbLayout.updateBangBoxesContaining(picked);
		width = newSize.width;
		height = newSize.height;
	}

	@Override
	public void setLocation(V picked, double x, double y) {
		super.setLocation(picked, x, y);
		Dimension newSize = bbLayout.updateBangBoxesContaining(picked);
		width = newSize.width;
		height = newSize.height;
	}
}
