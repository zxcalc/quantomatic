/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;

/**
 *
 * @author alex
 */
public abstract class AbstractBangBoxDotLayout<V,E,B>
	extends AbstractDotLayout<V,E>
	implements BangBoxLayout<V, E, B> {

	protected BangBoxLayoutManager<V,E,B> bbLayout;

	public AbstractBangBoxDotLayout(DirectedBangBoxGraph<V,E,B> graph, double vertexPadding, double bangBoxPadding) {
		super(graph, vertexPadding);
		bbLayout = new BangBoxLayoutManager<V, E, B>(this, bangBoxPadding);
	}

	@Override
	protected boolean isWorkToDo() {
		return super.isWorkToDo() || (((DirectedBangBoxGraph<V,E,B>)getGraph()).getBangBoxCount() > 0);
	}

	@Override
	@SuppressWarnings("unchecked")
	public DirectedBangBoxGraph<V, E, B> getGraph() {
		return (DirectedBangBoxGraph<V, E, B>)super.getGraph();
	}

	/**
	 * Get the !-box key to use to describe a !-box.
	 *
	 * This must be unique (within the graph) for each !-box, and
	 * must always return the same value for any given !-box between a
	 * call to beginLayout() and the corresponding call to endLayout().
	 *
	 * @return a string containing no double quote characters
	 */
	protected abstract String getBangBoxDotKey(B bangbox);

	protected void addBangBoxLines(StringBuilder g) {
		for (B b : getGraph().getBangBoxes()) {
			g.append("subgraph \"cluster ");
			g.append(getBangBoxDotKey(b));
			g.append("\" {\n");
			Collection<V> contents = getGraph().getBoxedVertices(b);
			if (contents.isEmpty()) {
				g.append("\"emptybb vert ");
				g.append(getBangBoxDotKey(b));
				g.append("\" [label=\"\"]; ");
			} else {
				for (V v : getGraph().getBoxedVertices(b)) {
					g.append("\"");
					g.append(getVertexDotKey(v));
					g.append("\"; ");
				}
			}
			g.append("\n}\n");
		}
	}

	@Override
	protected void addGraphContents(StringBuilder g) {
		addVertexLines(g);
		addEdgeLines(g);
		addBangBoxLines(g);
	}

	protected void layoutBangBoxes() {
		bbLayout.clear();
		for (B b : getGraph().getBangBoxes()) {
			if (getGraph().getBoxedVertices(b).isEmpty()) {
				bbLayout.setEmptyBangBoxLocation(b, vertexPositions.get("emptybb vert " + getBangBoxDotKey(b)));
			} else {
				bbLayout.recalculateNonEmptyBangBox(b);
			}
		}
	}

	@Override
	protected void layoutGraph() {
		for (V v : graph.getVertices()) {
			if (!isLocked(v))
				setLocationNoUpdates(v, vertexPositions.get(getVertexDotKey(v)));
		}

		layoutBangBoxes();
	}

	public Rectangle2D transformBangBox(B bb) {
		return bbLayout.transform(bb);
	}

	@Override
	public void setLocation(V picked, Point2D p) {
		super.setLocation(picked, p);
		size = bbLayout.updateBangBoxesContaining(picked);
	}

	@Override
	public void setLocation(V picked, double x, double y) {
		super.setLocation(picked, x, y);
		size = bbLayout.updateBangBoxesContaining(picked);
	}

	@Override
	public void recalculateSize() {
		double right = vertexSpacing;
		double bottom = vertexSpacing;
		for (V v : getGraph().getVertices()) {
			Point2D point = transform(v);
			right = Math.max(right, point.getX());
			bottom = Math.max(bottom, point.getY());
		}
		for (B b : getGraph().getBangBoxes()) {
			Rectangle2D rect = transformBangBox(b);
			right = Math.max(right, rect.getMaxX());
			bottom = Math.max(bottom, rect.getMaxY());
		}
		right += vertexSpacing;
		bottom += vertexSpacing;
		size.setSize(Math.ceil(right), Math.ceil(bottom));
	}
}
