/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization.control;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.PickingGraphMousePlugin;
import edu.uci.ics.jung.visualization.picking.PickedState;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.util.Set;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;

/**
 * Constrains the left and top drag movements of the picking graph mouse
 * plugin, to prevent nodes being dragged into negative co-ordinates.
 *
 * @author Alex Merry
 */
public class ConstrainedPickingGraphMousePlugin<V, E>
	extends PickingGraphMousePlugin<V, E>
{
	protected double leftConstraint = 0.0;
	protected double topConstraint = 0.0;
	protected double xDragBounce = 0.0;
	protected double yDragBounce = 0.0;
	protected ConstrainingAction constrainingAction = ConstrainingAction.StopMovement;

	/**
	 * What to do when a movement is inhibited because of the constraint
	 */
	public enum ConstrainingAction
	{
		/**
		 * Simply stops the movement from taking place
		 */
		StopMovement,
		/**
		 * Moves the rest of the graph away
		 */
		MoveOthers
	}

	public ConstrainedPickingGraphMousePlugin() {
	}

	/**
	 * Create a ConstrainedPickingGraphMousePlugin with a particular
	 * action to perform when movement is inhibited
	 *
	 * @param constrainingAction The action to perform when movement is
	 *                           inhibited by the constraints
	 */
	public ConstrainedPickingGraphMousePlugin(ConstrainingAction constrainingAction) {
		this.constrainingAction = constrainingAction;
	}

	/**
	 * Create a ConstrainedPickingGraphMousePlugin with specific left and
	 * top constraints (by default they are both 0.0).
	 *
	 * This is useful to provide a padding box around vertices.
	 *
	 * @param leftConstraint The furthest left a vertex may be dragged
	 * @param topConstraint The highest a vertex may be dragged
	 */
	public ConstrainedPickingGraphMousePlugin(double leftConstraint,
						  double topConstraint) {
		this.leftConstraint = leftConstraint;
		this.topConstraint = topConstraint;
	}

	/**
	 * Create a ConstrainedPickingGraphMousePlugin with specific left and
	 * top constraints (by default they are both 0.0) and a particular
	 * action to perform when movement is inhibited
	 *
	 * @param constrainingAction The action to perform when movement is
	 *                           inhibited by the constraints
	 * @param leftConstraint The furthest left a vertex may be dragged
	 * @param topConstraint The highest a vertex may be dragged
	 */
	public ConstrainedPickingGraphMousePlugin(
		ConstrainingAction constrainingAction,
		double leftConstraint,
		double topConstraint)
	{
		this.constrainingAction = constrainingAction;
		this.leftConstraint = leftConstraint;
		this.topConstraint = topConstraint;
	}

	private void moveNodes(VisualizationViewer<V, E> vv,
	                       double dx, double dy)
	{
		Layout<V, E> layout = vv.getGraphLayout();
		double odx = 0.0;
		double ody = 0.0;
		// if the mouse has moved without taking nodes
		// with it, because of the constraints, let it
		// move back to its starting point (relative to
		// the nodes) before moving again.
		if (dx > 0 && xDragBounce < 0)
		{
			double xfer = Math.min(dx, -xDragBounce);
			dx -= xfer;
			xDragBounce += xfer;
			odx -= xfer;
		}
		if (dy > 0 && yDragBounce < 0)
		{
			double xfer = Math.min(dy, -yDragBounce);
			dy -= xfer;
			yDragBounce += xfer;
			ody -= xfer;
		}
		PickedState<V> ps = vv.getPickedVertexState();
		Set<V> picked = ps.getPicked();

		double farLeft = Double.MAX_VALUE;
		double farTop = Double.MAX_VALUE;
		if (dx < 0 || dy < 0) {
			for (V v : picked) {
				Point2D vp = layout.transform(v);
				farLeft = Math.min(farLeft, vp.getX());
				farTop = Math.min(farTop, vp.getY());
			}
		}
		// record how far we moved without taking nodes
		// with us, so we can bounce back later
		if (farLeft + dx < leftConstraint) {
			double diff = leftConstraint - (farLeft + dx);
			xDragBounce -= diff;
			dx += diff;
			odx += diff;
		}
		if (farTop + dy < topConstraint) {
			double diff = topConstraint - (farTop + dy);
			yDragBounce -= diff;
			dy += diff;
			ody += diff;
		}
		if (constrainingAction == ConstrainingAction.StopMovement ||
			(odx == 0.0 && ody == 0.0))
		{
			for (V v : ps.getPicked()) {
				Point2D vp = layout.transform(v);
				vp.setLocation(vp.getX() + dx, vp.getY() + dy);
				layout.setLocation(v, vp);
			}
		}
		else
		{
			for (V v : vv.getGraphLayout().getGraph().getVertices()) {
				Point2D vp = layout.transform(v);
				if (picked.contains(v))
					vp.setLocation(vp.getX() + dx, vp.getY() + dy);
				else
					vp.setLocation(vp.getX() + odx, vp.getY() + ody);
				layout.setLocation(v, vp);
			}
		}
	}

	@Override
	@SuppressWarnings("unchecked")
	public void mouseDragged(MouseEvent e) {
		if (locked == false) {
			VisualizationViewer<V, E> vv = (VisualizationViewer<V, E>) e.getSource();
			if (vertex != null) {
				Point p = e.getPoint();
				Point2D graphPoint = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(p);
				Point2D graphDown = vv.getRenderContext().getMultiLayerTransformer().inverseTransform(down);
				double dx = graphPoint.getX() - graphDown.getX();
				double dy = graphPoint.getY() - graphDown.getY();
				moveNodes(vv, dx, dy);
				down = p;
				vv.revalidate();
			}
			else {
				Point2D out = e.getPoint();
				if (e.getModifiers() == this.addToSelectionModifiers
					|| e.getModifiers() == modifiers) {
					rect.setFrameFromDiagonal(down, out);
				}
			}
			if (vertex != null) {
				e.consume();
			}
			vv.repaint();
		}
	}
}
