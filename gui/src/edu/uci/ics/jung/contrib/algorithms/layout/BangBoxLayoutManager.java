/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.contrib.graph.DirectedBangBoxGraph;
import edu.uci.ics.jung.algorithms.layout.Layout;
import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alex
 */
public class BangBoxLayoutManager<V,E,B> implements Transformer<B, Rectangle2D>
{
	protected double bangBoxPadding;
	protected Map<B, Rectangle2D> bbRects = new HashMap<B, Rectangle2D>();
	protected Layout<V,E> mainLayout;

	public BangBoxLayoutManager(Layout<V, E> mainLayout, double bangBoxPadding) {
		this.mainLayout = mainLayout;
		this.bangBoxPadding = bangBoxPadding;
	}

	public Rectangle2D transform(B i) {
		return bbRects.get(i);
	}

	public double getBangBoxPadding() {
		return bangBoxPadding;
	}

	public void setBangBoxPadding(double bangBoxPadding) {
		this.bangBoxPadding = bangBoxPadding;
	}

	@SuppressWarnings("unchecked")
	private DirectedBangBoxGraph<V, E, B> getGraph() {
		return (DirectedBangBoxGraph<V, E, B>)mainLayout.getGraph();
	}

	public void recalculateNonEmptyBangBox(B bb) {
		Collection<V> contents = getGraph().getBoxedVertices(bb);
		Rectangle2D rect = null;
		for (V v : contents) {
			Point2D p = mainLayout.transform(v);
			if (rect == null)
				rect = new Rectangle2D.Double(p.getX(),p.getY(),0,0);
			else
				rect.add(p);
		}
		rect.setRect(rect.getX()-bangBoxPadding,
			     rect.getY()-bangBoxPadding,
			     rect.getWidth()+(2* bangBoxPadding),
			     rect.getHeight()+(2* bangBoxPadding));
		bbRects.put(bb, rect);
	}

	public void setEmptyBangBoxLocation(B bb, Point2D pos) {
		Rectangle2D rect = new Rectangle2D.Double();
		rect.setRect(pos.getX() - bangBoxPadding,
			     pos.getY() - bangBoxPadding,
			     2 * bangBoxPadding,
			     2 * bangBoxPadding);
		bbRects.put(bb, rect);
	}

	public void clear() {
		bbRects.clear();
	}

	public Dimension updateBangBoxesContaining(V picked) {
		Dimension size = mainLayout.getSize();
		for (B b : getGraph().getBangBoxes()) {
			if (getGraph().getBoxedVertices(b).contains(picked)) {
				recalculateNonEmptyBangBox(b);
				Rectangle2D rect = transform(b);
				if (rect.getMaxX() + bangBoxPadding > size.width) {
					size.width = (int)Math.ceil(rect.getMaxX() + bangBoxPadding);
				}
				if (rect.getMaxY() + bangBoxPadding > size.height) {
					size.height = (int)Math.ceil(rect.getMaxY() + bangBoxPadding);
				}
			}
		}
		return size;
	}
}
