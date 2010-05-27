/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib;

import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.AbstractGraphMousePlugin;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

/**
 *
 * @author alex
 */
public class ViewScrollingGraphMousePlugin extends AbstractGraphMousePlugin
	implements MouseWheelListener {

	public enum ModiferStrictness {
		Exact,
		All,
		Any
	}
	private ModiferStrictness strictness;
	private double shift = 1.0;

	public ViewScrollingGraphMousePlugin() {
		this(0, ModiferStrictness.Exact);
	}

	public ViewScrollingGraphMousePlugin(int modifiers) {
		this(modifiers, ModiferStrictness.Exact);
	}

	public ViewScrollingGraphMousePlugin(int modifiers, ModiferStrictness strictness) {
		super(modifiers);
		this.strictness = strictness;
	}

	@Override
	public boolean checkModifiers(MouseEvent e) {
		switch (strictness) {
			case Exact:
				return e.getModifiers() == modifiers;
			case All:
				return (e.getModifiers() & modifiers) == modifiers;
			case Any:
				return (e.getModifiers() & modifiers) != 0;
		}
		// shouldn't get this:
		return false;
	}

	/**
	 * Get the amount a mouse wheel "click" moves the view by
	 *
	 * @return The per-click shift amount
	 */
	public double getShift() {
		return shift;
	}

	/**
	 * Set the amount a mouse wheel "click" moves the view by
	 *
	 * @param shift The per-click shift amount
	 */
	public void setShift(double shift) {
		this.shift = shift;
	}

	public void mouseWheelMoved(MouseWheelEvent e) {
		if (checkModifiers(e)) {
			VisualizationViewer vv = (VisualizationViewer) e.getSource();
			MutableTransformer viewTransformer = vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);

			int amount = e.getWheelRotation();
			// negative wheel rotation, so that scrolling
			// down shifts the view up
			double dy = shift * (-amount);
			viewTransformer.translate(0, dy);

			e.consume();
			vv.repaint();
		}
	}
}
